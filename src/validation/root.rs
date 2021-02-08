use super::super::FileProvider;
use super::arena::*;
use super::format::*;
use crate::parse::{ast, root};
use crate::Range;

use std::collections::HashMap;
use std::io;
use std::path::Path;

use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use super::error::{ItemType, ValidationError, ValidationErrorType};

use crate::identifier::Identifier;

use std::convert::From;

use nom::error::convert_error;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
#[allow(clippy::type_complexity)]
pub struct Root {
    registries: HashMap<
        Identifier,
        (
            HashMap<Identifier, Index<CompoundTag>>,
            Option<Index<CompoundTag>>,
        ),
    >,

    root_modules: HashMap<String, Index<Module>>,

    compound_arena: Arena<CompoundTag>,
    enum_arena: Arena<EnumItem>,
    module_arena: Arena<Module>,

    unresolved_inject: Vec<InjectType>,
}

impl Root {
    pub fn new() -> Self {
        Root {
            registries: HashMap::new(),

            root_modules: HashMap::new(),

            compound_arena: Arena::new(),
            enum_arena: Arena::new(),
            module_arena: Arena::new(),

            unresolved_inject: vec![],
        }
    }

    #[allow(clippy::type_complexity)]
    pub fn get_registry(
        &self,
        name: &Identifier,
    ) -> Option<&(
        HashMap<Identifier, Index<CompoundTag>>,
        Option<Index<CompoundTag>>,
    )> {
        self.registries.get(name)
    }

    pub fn get_regitry_item(&self, name: &Identifier, id: &Identifier) -> Option<&CompoundTag> {
        let (r, d) = self.registries.get(name)?;
        Some(&self.compound_arena[*r.get(id).unwrap_or(&(*d)?)])
    }

    pub fn get_compound(&self, name: Index<CompoundTag>) -> &CompoundTag {
        &self.compound_arena[name]
    }

    pub fn get_module(&self, name: Index<Module>) -> &Module {
        &self.module_arena[name]
    }

    pub fn get_enum(&self, name: Index<EnumItem>) -> &EnumItem {
        &self.enum_arena[name]
    }

    pub fn get_root_module(&self, name: &str) -> Option<Index<Module>> {
        self.root_modules.get(name).cloned()
    }

    pub fn get_modules(&self) -> std::slice::Iter<Module> {
        self.module_arena.iter()
    }

    pub fn get_compounds(&self) -> std::slice::Iter<CompoundTag> {
        self.compound_arena.iter()
    }

    pub fn get_enums(&self) -> std::slice::Iter<EnumItem> {
        self.enum_arena.iter()
    }

    /// `p` needs to be an absolute path, and must be UTF-8
    pub fn add_root_module<F, P>(&mut self, p: P, fp: &F) -> Result<(), NbtDocError>
    where
        F: FileProvider,
        P: AsRef<Path>,
    {
        let module_name = p
            .as_ref()
            .file_name()
            .ok_or_else(|| io::Error::from(io::ErrorKind::NotFound))?
            .to_str()
            .unwrap();
        let module_tree = ModuleTree::read(p.as_ref(), "mod", fp)?;
        let root = self.register_module(Module {
            children: HashMap::new(),
            parent: None,
        });
        self.root_modules.insert(String::from(module_name), root);
        self.register_module_tree(root, &module_tree)?;
        self.preresolve_module_tree(root, &module_tree, &[module_name])?;
        self.resolve_module_tree(root, module_tree, &[module_name])?;
        self.postresolve_module_tree()?;
        Ok(())
    }

    pub fn add_root_modules<F, P>(&mut self, paths: &[P], fp: &F) -> Result<(), NbtDocError>
    where
        F: FileProvider,
        P: AsRef<Path>,
    {
        let module_infos = paths
            .iter()
            .map(|p| {
                let name = p
                    .as_ref()
                    .file_name()
                    .ok_or_else(|| io::Error::from(io::ErrorKind::NotFound))?
                    .to_str()
                    .unwrap();
                let tree = ModuleTree::read(p.as_ref(), "mod", fp)?;
                let root = self.register_module(Module {
                    children: HashMap::new(),
                    parent: None,
                });
                self.root_modules.insert(String::from(name), root);
                Result::<_, NbtDocError>::Ok((name, tree, root))
            })
            .collect::<Result<Vec<_>, _>>()?;
        for (name, tree, root) in module_infos.iter() {
            self.preresolve_module_tree(*root, tree, &[name])?;
        }
        for (name, tree, root) in module_infos.into_iter() {
            self.resolve_module_tree(root, tree, &[name])?;
        }
        self.postresolve_module_tree()?;
        Ok(())
    }

    fn register_module_tree(
        &mut self,
        rootind: Index<Module>,
        tree: &ModuleTree,
    ) -> Result<(), ValidationError> {
        let cast = &tree.val;
        // first register items so lower modules can resolve
        for (n, _) in cast.compounds.iter() {
            let ind = self.register_compound(CompoundTag {
                description: String::new(),
                supers: None,
                fields: HashMap::new(),
            });
            self.module_arena[rootind]
                .children
                .insert(n.clone(), ItemIndex::Compound(ind));
        }
        for (n, e) in cast.enums.iter() {
            let ind = self.register_enum(EnumItem {
                description: String::new(),
                et: match e.values {
                    ast::EnumType::Byte(_) => EnumType::Byte(HashMap::new()),
                    ast::EnumType::Short(_) => EnumType::Short(HashMap::new()),
                    ast::EnumType::Int(_) => EnumType::Int(HashMap::new()),
                    ast::EnumType::Long(_) => EnumType::Long(HashMap::new()),
                    ast::EnumType::Float(_) => EnumType::Float(HashMap::new()),
                    ast::EnumType::Double(_) => EnumType::Double(HashMap::new()),
                    ast::EnumType::String(_) => EnumType::String(HashMap::new()),
                },
            });
            self.module_arena[rootind]
                .children
                .insert(n.clone(), ItemIndex::Enum(ind));
        }
        // register modules, which will register their items
        let mut next = Vec::with_capacity(tree.children.len());
        for (n, m) in tree.children.iter() {
            let root = self.register_module(Module {
                children: HashMap::new(),
                parent: Some(rootind),
            });
            self.module_arena[rootind]
                .children
                .insert(n.clone(), ItemIndex::Module(root));
            next.push((root, m));
        }
        for (root, m) in next.into_iter() {
            self.register_module_tree(root, &m)?;
        }
        Ok(())
    }

    fn preresolve_module_tree(
        &mut self,
        rootind: Index<Module>,
        tree: &ModuleTree,
        module: &[&str],
    ) -> Result<(), ValidationError> {
        let cast = &tree.val;
        let eb = |x| ValidationError::new(module.iter().map(|x| String::from(*x)).collect(), x);
        for (e, n) in cast.uses.iter() {
            if *e {
                let last = match n
                    .last()
                    .ok_or_else(|| eb(ValidationErrorType::RootAccess))?
                {
                    ast::PathPart::Regular(v) => v,
                    ast::PathPart::Super => return Err(eb(ValidationErrorType::SuperImport)),
                    ast::PathPart::Root => return Err(eb(ValidationErrorType::RootAccess)),
                };
                let item = self.get_item_path(&n, Some(rootind), &HashMap::new(), module)?;
                if let ItemIndex::Module(_) = item {
                    return Err(eb(ValidationErrorType::InvalidType {
                        name: match n.last().unwrap() {
                            ast::PathPart::Regular(v) => v.clone(),
                            _ => panic!(),
                        },
                        ex: vec![ItemType::Compound, ItemType::Enum],
                        ty: ItemType::Module,
                    }));
                }
                self.module_arena[rootind]
                    .children
                    .insert(last.clone(), item);
            }
        }
        for (n, v) in &tree.children {
            let mut m = Vec::with_capacity(module.len() + 1);
            m.extend(module);
            m.push(n.as_ref());
            self.preresolve_module_tree(
                match &self.module_arena[rootind].children[n] {
                    ItemIndex::Module(v) => *v,
                    _ => panic!(),
                },
                v,
                &m,
            )?;
        }
        Ok(())
    }

    fn resolve_module_tree(
        &mut self,
        rootind: Index<Module>,
        tree: ModuleTree,
        module: &[&str],
    ) -> Result<(), ValidationError> {
        let cast = tree.val;
        let eb = |x| ValidationError::new(module.iter().map(|x| String::from(*x)).collect(), x);
        let mut imports = HashMap::new();
        for (_, n) in cast.uses {
            imports.insert(
                match n
                    .last()
                    .ok_or_else(|| eb(ValidationErrorType::RootAccess))?
                {
                    ast::PathPart::Regular(s) => s.clone(),
                    ast::PathPart::Root => return Err(eb(ValidationErrorType::RootAccess)),
                    ast::PathPart::Super => return Err(eb(ValidationErrorType::SuperImport)),
                },
                self.get_item_path(&n, Some(rootind), &HashMap::new(), module)?,
            );
        }
        for (n, c) in cast.compounds {
            // this better work
            let cpdi = *match self.module_arena[rootind].children.get(&n).unwrap() {
                ItemIndex::Compound(v) => v,
                _ => panic!(),
            };
            self.compound_arena[cpdi].description = c.description;
            match c.extend {
                Some(ast::CompoundSuper::Compound(v)) => {
                    self.compound_arena[cpdi].supers = Some(
                        match self.get_item_path(&v, Some(rootind), &imports, module)? {
                            ItemIndex::Compound(v) => CompoundExtend::Compound(v),
                            v => {
                                return Err(eb(ValidationErrorType::InvalidType {
                                    name: n,
                                    ex: vec![ItemType::Compound],
                                    ty: match v {
                                        ItemIndex::Enum(_) => ItemType::Enum,
                                        ItemIndex::Module(_) => ItemType::Module,
                                        ItemIndex::Compound(_) => panic!(),
                                    },
                                }))
                            }
                        },
                    )
                }
                Some(ast::CompoundSuper::Registry { target, path }) => {
                    self.compound_arena[cpdi].supers =
                        Some(CompoundExtend::Registry { target, path })
                }
                None => self.compound_arena[cpdi].supers = None,
            };
            for (n, t) in c.fields {
                let field = Field {
                    description: t.description,
                    optional: t.optional,
                    nbttype: self.convert_field_type(t.field_type, rootind, &imports, module)?,
                };
                self.compound_arena[cpdi].fields.insert(n, field);
            }
        }
        for (n, e) in cast.enums {
            let eni = *match self.module_arena[rootind].children.get(&n).unwrap() {
                ItemIndex::Enum(v) => v,
                _ => panic!(),
            };
            self.enum_arena[eni].description = e.description;
            self.enum_arena[eni].et = match e.values {
                ast::EnumType::Byte(v) => EnumType::Byte(
                    v.into_iter()
                        .map(|(n, v)| {
                            (
                                n,
                                EnumOption {
                                    description: v.description,
                                    value: v.value,
                                },
                            )
                        })
                        .collect(),
                ),
                ast::EnumType::Short(v) => EnumType::Short(
                    v.into_iter()
                        .map(|(n, v)| {
                            (
                                n,
                                EnumOption {
                                    description: v.description,
                                    value: v.value,
                                },
                            )
                        })
                        .collect(),
                ),
                ast::EnumType::Int(v) => EnumType::Int(
                    v.into_iter()
                        .map(|(n, v)| {
                            (
                                n,
                                EnumOption {
                                    description: v.description,
                                    value: v.value,
                                },
                            )
                        })
                        .collect(),
                ),
                ast::EnumType::Long(v) => EnumType::Long(
                    v.into_iter()
                        .map(|(n, v)| {
                            (
                                n,
                                EnumOption {
                                    description: v.description,
                                    value: v.value,
                                },
                            )
                        })
                        .collect(),
                ),
                ast::EnumType::Float(v) => EnumType::Float(
                    v.into_iter()
                        .map(|(n, v)| {
                            (
                                n,
                                EnumOption {
                                    description: v.description,
                                    value: v.value,
                                },
                            )
                        })
                        .collect(),
                ),
                ast::EnumType::Double(v) => EnumType::Double(
                    v.into_iter()
                        .map(|(n, v)| {
                            (
                                n,
                                EnumOption {
                                    description: v.description,
                                    value: v.value,
                                },
                            )
                        })
                        .collect(),
                ),
                ast::EnumType::String(v) => EnumType::String(
                    v.into_iter()
                        .map(|(n, v)| {
                            (
                                n,
                                EnumOption {
                                    description: v.description,
                                    value: v.value,
                                },
                            )
                        })
                        .collect(),
                ),
            }
        }
        for (p, d) in cast.describes {
            let target = match self.get_item_path(&p, Some(rootind), &imports, module)? {
                ItemIndex::Compound(v) => v,
                v => {
                    return Err(eb(ValidationErrorType::InvalidType {
                        name: match p
                            .last()
                            .ok_or_else(|| eb(ValidationErrorType::RootAccess))?
                        {
                            ast::PathPart::Root => String::from("root"),
                            ast::PathPart::Super => String::from("super"),
                            ast::PathPart::Regular(v) => v.clone(),
                        },
                        ex: vec![ItemType::Compound],
                        ty: match v {
                            ItemIndex::Enum(_) => ItemType::Enum,
                            ItemIndex::Module(_) => ItemType::Module,
                            ItemIndex::Compound(_) => panic!(),
                        },
                    }))
                }
            };
            let (ref mut dt, ref mut def) = {
                if !self.registries.contains_key(&d.describe_type) {
                    self.registries
                        .insert(d.describe_type.clone(), (HashMap::new(), None));
                };
                self.registries.get_mut(&d.describe_type).unwrap()
            };
            if let Some(targets) = d.targets {
                for n in targets {
                    if dt.contains_key(&n) {
                        return Err(eb(ValidationErrorType::DuplicateDescribe {
                            reg: d.describe_type,
                            t: Some(n),
                        }));
                    }
                    dt.insert(n, target);
                }
            } else {
                if def.is_some() {
                    return Err(eb(ValidationErrorType::DuplicateDescribe {
                        reg: d.describe_type,
                        t: None,
                    }));
                }
                *def = Some(target);
            }
        }
        for ast::InjectDef { target, ty } in cast.injects {
            let uj = match ty {
                ast::InjectType::Compound(v) => UnresolvedInject::Compound(
                    v.into_iter()
                        .map(|(s, f)| {
                            Ok((
                                s,
                                Field {
                                    description: f.description,
                                    optional: f.optional,
                                    nbttype: self.convert_field_type(
                                        f.field_type,
                                        rootind,
                                        &imports,
                                        module,
                                    )?,
                                },
                            ))
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                ),
                ast::InjectType::Enum(v) => UnresolvedInject::Enum(match v {
                    ast::EnumType::Byte(v) => EnumType::Byte(
                        v.into_iter()
                            .map(|(n, v)| {
                                (
                                    n,
                                    EnumOption {
                                        description: v.description,
                                        value: v.value,
                                    },
                                )
                            })
                            .collect(),
                    ),
                    ast::EnumType::Short(v) => EnumType::Short(
                        v.into_iter()
                            .map(|(n, v)| {
                                (
                                    n,
                                    EnumOption {
                                        description: v.description,
                                        value: v.value,
                                    },
                                )
                            })
                            .collect(),
                    ),
                    ast::EnumType::Int(v) => EnumType::Int(
                        v.into_iter()
                            .map(|(n, v)| {
                                (
                                    n,
                                    EnumOption {
                                        description: v.description,
                                        value: v.value,
                                    },
                                )
                            })
                            .collect(),
                    ),
                    ast::EnumType::Long(v) => EnumType::Long(
                        v.into_iter()
                            .map(|(n, v)| {
                                (
                                    n,
                                    EnumOption {
                                        description: v.description,
                                        value: v.value,
                                    },
                                )
                            })
                            .collect(),
                    ),
                    ast::EnumType::Float(v) => EnumType::Float(
                        v.into_iter()
                            .map(|(n, v)| {
                                (
                                    n,
                                    EnumOption {
                                        description: v.description,
                                        value: v.value,
                                    },
                                )
                            })
                            .collect(),
                    ),
                    ast::EnumType::Double(v) => EnumType::Double(
                        v.into_iter()
                            .map(|(n, v)| {
                                (
                                    n,
                                    EnumOption {
                                        description: v.description,
                                        value: v.value,
                                    },
                                )
                            })
                            .collect(),
                    ),
                    ast::EnumType::String(v) => EnumType::String(
                        v.into_iter()
                            .map(|(n, v)| {
                                (
                                    n,
                                    EnumOption {
                                        description: v.description,
                                        value: v.value,
                                    },
                                )
                            })
                            .collect(),
                    ),
                }),
            };
            match self.get_item_path(&target, Some(rootind), &imports, module) {
                Ok(v) => self.unresolved_inject.push(InjectType::Registered(
                    v,
                    uj,
                    match target.last().unwrap() {
                        ast::PathPart::Regular(v) => v.clone(),
                        ast::PathPart::Super => {
                            return Err(eb(ValidationErrorType::InvalidType {
                                ex: vec![ItemType::Compound, ItemType::Enum],
                                ty: ItemType::Module,
                                name: String::from(*module.last().unwrap_or(&"::")),
                            }))
                        }
                        ast::PathPart::Root => return Err(eb(ValidationErrorType::RootAccess)),
                    },
                )),
                Err(ValidationError {
                    err: ValidationErrorType::UnresolvedName(_),
                    ..
                }) => {
                    let mut dir: Vec<String> = module.iter().map(|x| String::from(*x)).collect();
                    for x in &target {
                        match x {
                            ast::PathPart::Regular(v) => dir.push(v.clone()),
                            ast::PathPart::Super => {
                                dir.pop();
                            }
                            ast::PathPart::Root => dir.clear(),
                        };
                    }
                    self.unresolved_inject
                        .push(InjectType::Unregistered(dir, uj));
                }
                Err(v) => return Err(v),
            }
        }
        for (n, v) in tree.children {
            let mut m = Vec::with_capacity(module.len() + 1);
            m.extend(module);
            m.push(n.as_ref());
            self.resolve_module_tree(
                match self.module_arena[rootind].children[&n] {
                    ItemIndex::Module(v) => v,
                    _ => panic!(),
                },
                v,
                &m,
            )?;
        }
        Ok(())
    }

    fn postresolve_module_tree(&mut self) -> Result<(), ValidationError> {
        let eb = |x, b: &[String]| ValidationError::new(Vec::from(b), x);
        let mut rest = Vec::with_capacity(self.unresolved_inject.len());
        'outer: for v in self.unresolved_inject.drain(..) {
            let (it, n, m) = match v {
                InjectType::Registered(i, u, n) => (i, u, n.clone()),
                InjectType::Unregistered(p, u) => (
                    {
                        let mut iter = p.iter();
                        let mut module = match self.root_modules.get(
                            iter.next()
                                .ok_or_else(|| eb(ValidationErrorType::RootAccess, &[]))?,
                        ) {
                            Some(v) => *v,
                            None => {
                                rest.push(InjectType::Unregistered(p, u));
                                continue;
                            }
                        };
                        loop {
                            let n = match iter.next() {
                                Some(v) => v,
                                None => {
                                    rest.push(InjectType::Unregistered(p, u));
                                    continue 'outer;
                                }
                            };
                            module = match self.module_arena[module].children.get(n) {
                                Some(v) => match *v {
                                    ItemIndex::Module(v) => v,
                                    v => break v,
                                },
                                None => {
                                    rest.push(InjectType::Unregistered(p, u));
                                    continue 'outer;
                                }
                            }
                        }
                    },
                    u,
                    p.last().unwrap().clone(),
                ),
            };
            match (it, n) {
                (ItemIndex::Compound(i), UnresolvedInject::Compound(v)) => {
                    self.compound_arena[i].fields.extend(v)
                }
                (ItemIndex::Enum(enum_index), UnresolvedInject::Enum(enum_type)) => {
                    match (&mut self.enum_arena[enum_index].et, enum_type) {
                        (EnumType::Byte(dest), EnumType::Byte(ref mut src)) => {
                            dest.extend(src.drain())
                        }
                        (EnumType::Short(dest), EnumType::Short(ref mut src)) => {
                            dest.extend(src.drain())
                        }
                        (EnumType::Int(dest), EnumType::Int(ref mut src)) => {
                            dest.extend(src.drain())
                        }
                        (EnumType::Long(dest), EnumType::Long(ref mut src)) => {
                            dest.extend(src.drain())
                        }
                        (EnumType::Float(dest), EnumType::Float(ref mut src)) => {
                            dest.extend(src.drain())
                        }
                        (EnumType::Double(dest), EnumType::Double(ref mut src)) => {
                            dest.extend(src.drain())
                        }
                        (EnumType::String(dest), EnumType::String(ref mut src)) => {
                            dest.extend(src.drain())
                        }
                        (d, s) => {
                            return Err(eb(
                                ValidationErrorType::MismatchedEnum {
                                    ex: super::error::EnumType::from(&*d),
                                    ty: super::error::EnumType::from(&s),
                                    name: m,
                                },
                                &[],
                            ))
                        }
                    }
                }
                (ItemIndex::Module(_), _) => {
                    return Err(ValidationError::new(
                        vec![],
                        ValidationErrorType::InvalidType {
                            ex: vec![ItemType::Enum, ItemType::Compound],
                            name: m,
                            ty: ItemType::Module,
                        },
                    ))
                }
                _ => {
                    return Err(ValidationError::new(
                        vec![],
                        ValidationErrorType::UnresolvedName(m),
                    ))
                }
            };
        }
        self.unresolved_inject = rest;
        Ok(())
    }

    fn get_item_path(
        &self,
        path: &[ast::PathPart],
        rel: Option<Index<Module>>,
        imports: &HashMap<String, ItemIndex>,
        module: &[&str],
    ) -> Result<ItemIndex, ValidationError> {
        let eb = |x| ValidationError::new(module.iter().map(|x| String::from(*x)).collect(), x);
        if path.is_empty() {
            return Err(eb(ValidationErrorType::RootAccess));
        }
        let mut start = true;
        let mut current = rel;
        for part in &path[0..path.len() - 1] {
            current = match self.get_child(
                part,
                current,
                if start {
                    start = false;
                    Some(imports)
                } else {
                    None
                },
                module,
            )? {
                None => None,
                Some(v) => match v {
                    ItemIndex::Module(m) => Some(m),
                    v => {
                        return Err(eb(ValidationErrorType::InvalidType {
                            name: match part {
                                ast::PathPart::Regular(v) => v.clone(),
                                // Super **must** lead to a module, and Root has already been covered
                                _ => panic!(),
                            },
                            ex: vec![ItemType::Module],
                            ty: match v {
                                ItemIndex::Compound(_) => ItemType::Compound,
                                ItemIndex::Enum(_) => ItemType::Enum,
                                ItemIndex::Module(_) => panic!(),
                            },
                        }))
                    }
                },
            }
        }
        self.get_child(
            path.last().unwrap(),
            current,
            if start { Some(imports) } else { None },
            module,
        )?
        .ok_or_else(|| eb(ValidationErrorType::RootAccess))
    }

    fn get_child(
        &self,
        part: &ast::PathPart,
        path: Option<Index<Module>>,
        imports: Option<&HashMap<String, ItemIndex>>,
        module: &[&str],
    ) -> Result<Option<ItemIndex>, ValidationError> {
        let eb = |x| ValidationError::new(module.iter().map(|x| String::from(*x)).collect(), x);
        Ok(match part {
            ast::PathPart::Root => None,
            ast::PathPart::Super => self.module_arena
                [path.ok_or_else(|| eb(ValidationErrorType::RootAccess))?]
            .parent
            .map(ItemIndex::Module),
            ast::PathPart::Regular(v) => Some(
                match path {
                    Some(i) => self.module_arena[i]
                        .children
                        .get(v.as_str())
                        .cloned()
                        .or_else(|| imports.and_then(|h| h.get(v.as_str())).cloned()),
                    None => self
                        .root_modules
                        .get(v.as_str())
                        .map(|v| ItemIndex::Module(*v)),
                }
                .ok_or_else(|| eb(ValidationErrorType::UnresolvedName(v.clone())))?,
            ),
        })
    }

    fn register_module(&mut self, module: Module) -> Index<Module> {
        self.module_arena.push(module)
    }

    fn register_compound(&mut self, item: CompoundTag) -> Index<CompoundTag> {
        self.compound_arena.push(item)
    }

    fn register_enum(&mut self, item: EnumItem) -> Index<EnumItem> {
        self.enum_arena.push(item)
    }

    fn convert_field_type(
        &self,
        ft: ast::FieldType,
        root: Index<Module>,
        imports: &HashMap<String, ItemIndex>,
        module: &[&str],
    ) -> Result<NbtValue, ValidationError> {
        let eb = |x| ValidationError::new(module.iter().map(|x| String::from(*x)).collect(), x);
        Ok(match ft {
            ast::FieldType::BooleanType => NbtValue::Boolean,
            ast::FieldType::StringType => NbtValue::String,
            ast::FieldType::NamedType(v) => {
                let item = self.get_item_path(&v, Some(root), imports, module)?;
                match item {
                    ItemIndex::Module(_) => {
                        return Err(eb(ValidationErrorType::InvalidType {
                            name: match v.last().unwrap() {
                                ast::PathPart::Regular(v) => v.clone(),
                                ast::PathPart::Root => String::from("root"),
                                ast::PathPart::Super => String::from("super"),
                            },
                            ex: vec![ItemType::Compound, ItemType::Enum],
                            ty: ItemType::Module,
                        }))
                    }
                    ItemIndex::Compound(v) => NbtValue::Compound(v),
                    ItemIndex::Enum(v) => NbtValue::Enum(v),
                }
            }
            ast::FieldType::ArrayType(v) => match v {
                ast::NumberArrayType::Byte {
                    value_range,
                    len_range,
                } => NbtValue::ByteArray(NumberArrayTag {
                    length_range: len_range.map(|x| convert_range(x, 0, i32::max_value())),
                    value_range: value_range
                        .map(|x| convert_range(x, i8::min_value(), i8::max_value())),
                }),
                ast::NumberArrayType::Int {
                    value_range,
                    len_range,
                } => NbtValue::IntArray(NumberArrayTag {
                    length_range: len_range.map(|x| convert_range(x, 0, i32::max_value())),
                    value_range: value_range
                        .map(|x| convert_range(x, i32::min_value(), i32::max_value())),
                }),
                ast::NumberArrayType::Long {
                    value_range,
                    len_range,
                } => NbtValue::LongArray(NumberArrayTag {
                    length_range: len_range.map(|x| convert_range(x, 0, i32::max_value())),
                    value_range: value_range
                        .map(|x| convert_range(x, i64::min_value(), i64::max_value())),
                }),
            },
            ast::FieldType::NumberType(v) => match v {
                ast::NumberPrimitiveType::Byte(range) => NbtValue::Byte(NumberTag {
                    range: range.map(|x| convert_range(x, i8::min_value(), i8::max_value())),
                }),
                ast::NumberPrimitiveType::Short(range) => NbtValue::Short(NumberTag {
                    range: range.map(|x| convert_range(x, i16::min_value(), i16::max_value())),
                }),
                ast::NumberPrimitiveType::Int(range) => NbtValue::Int(NumberTag {
                    range: range.map(|x| convert_range(x, i32::min_value(), i32::max_value())),
                }),
                ast::NumberPrimitiveType::Long(range) => NbtValue::Long(NumberTag {
                    range: range.map(|x| convert_range(x, i64::min_value(), i64::max_value())),
                }),
                ast::NumberPrimitiveType::Float(range) => NbtValue::Float(NumberTag {
                    range: range
                        .map(|x| convert_range(x, std::f32::NEG_INFINITY, std::f32::INFINITY)),
                }),
                ast::NumberPrimitiveType::Double(range) => NbtValue::Double(NumberTag {
                    range: range
                        .map(|x| convert_range(x, std::f64::NEG_INFINITY, std::f64::INFINITY)),
                }),
            },
            ast::FieldType::ListType {
                item_type,
                len_range,
            } => NbtValue::List {
                length_range: len_range.map(|x| convert_range(x, 0, i32::max_value())),
                value_type: Box::from(self.convert_field_type(*item_type, root, imports, module)?),
            },
            ast::FieldType::IndexType { target, path } => NbtValue::Index { path, target },
            ast::FieldType::IdType(v) => NbtValue::Id(v),
            ast::FieldType::OrType(v) => NbtValue::Or(
                v.into_iter()
                    .map(|x| self.convert_field_type(x, root, imports, module))
                    .collect::<Result<Vec<NbtValue>, ValidationError>>()?,
            ),
        })
    }
}

impl Default for Root {
    fn default() -> Self {
        Self::new()
    }
}

fn convert_range<T: Copy>(range: ast::Range<T>, min: T, max: T) -> Range<T> {
    match range {
        ast::Range::Single(v) => Range(v, v),
        ast::Range::Both(l, h) => Range(l, h),
        ast::Range::Low(l) => Range(l, max),
        ast::Range::High(h) => Range(min, h),
    }
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
enum InjectType {
    Unregistered(Vec<String>, UnresolvedInject),
    Registered(ItemIndex, UnresolvedInject, String),
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
enum UnresolvedInject {
    Compound(Vec<(String, Field)>),
    Enum(EnumType),
}

struct ModuleTree {
    val: ast::NbtDocFile,
    children: HashMap<String, ModuleTree>,
}

impl ModuleTree {
    pub fn read<P, F>(dir: P, name: &str, fp: &F) -> Result<Self, NbtDocError>
    where
        P: AsRef<Path>,
        F: FileProvider,
    {
        let dir = dir.as_ref();
        let file = fp.read_file(dir.join(format!("{}.nbtdoc", name)))?;
        let mut out = ModuleTree {
            val: match root::<nom::error::VerboseError<&str>>(&file) {
                Ok(v) => v,
                Err(e) => {
                    return Err(match e {
                        nom::Err::Error(e) | nom::Err::Failure(e) => {
                            NbtDocError::Parse(convert_error(&file, e))
                        }
                        nom::Err::Incomplete(e) => NbtDocError::Parse(match e {
                            nom::Needed::Size(e) => format!("Needs {} more bytes of data", e),
                            nom::Needed::Unknown => "Needs unknown number of bytes".to_string(),
                        }),
                    })
                }
            }
            .1,
            children: HashMap::new(),
        };
        for x in out.val.mods.iter() {
            let fname = format!("{}.nbtdoc", x);
            out.children.insert(
                x.clone(),
                match name {
                    "mod" => {
                        if fp.exists(dir.join(fname)) {
                            ModuleTree::read(dir, x.as_str(), fp)?
                        } else {
                            ModuleTree::read(dir.join(x), "mod", fp)?
                        }
                    }
                    v => {
                        if fp.exists(dir.join(v).join(fname)) {
                            ModuleTree::read(dir.join(v), x.as_str(), fp)?
                        } else {
                            ModuleTree::read(dir.join(v).join(x), "mod", fp)?
                        }
                    }
                },
            );
        }
        Ok(out)
    }
}

#[derive(Debug)]
pub enum NbtDocError {
    Io(io::Error),
    Parse(String),
    Validation(ValidationError),
}

impl Display for NbtDocError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Io(e) => write!(f, "{}", e),
            Self::Parse(e) => write!(f, "{}", e),
            Self::Validation(e) => write!(f, "{}", e),
        }
    }
}

impl From<io::Error> for NbtDocError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<ValidationError> for NbtDocError {
    fn from(e: ValidationError) -> Self {
        Self::Validation(e)
    }
}

impl Error for NbtDocError {}

#[cfg(test)]
mod tests {

    use super::*;
    use std::path::PathBuf;

    struct MockFileProvider {
        map: HashMap<PathBuf, &'static str>,
    }

    impl FileProvider for MockFileProvider {
        fn read_file<F: AsRef<Path>>(&self, f: F) -> io::Result<String> {
            Ok(String::from(*self.map.get(f.as_ref()).unwrap()))
        }

        fn exists<F: AsRef<Path>>(&self, f: F) -> bool {
            self.map.contains_key(&PathBuf::from(f.as_ref()))
        }
    }

    #[test]
    fn small_files() -> Result<(), NbtDocError> {
        let mut fp = MockFileProvider {
            map: HashMap::new(),
        };
        fp.map.insert(
            PathBuf::from("/mymod/mod.nbtdoc"),
            include_str!("../../tests/small_file_root.nbtdoc"),
        );
        fp.map.insert(
            PathBuf::from("/mymod/small_file_sibling.nbtdoc"),
            include_str!("../../tests/small_file_sibling.nbtdoc"),
        );
        let mut root = Root::new();
        root.add_root_module("/mymod", &fp)?;
        Ok(())
    }
}
