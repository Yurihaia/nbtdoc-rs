use std::path::{
	Path
};

use std::fs;
use std::io;

pub trait FileProvider {
	fn read_file<P: AsRef<Path>>(&self, path: P) -> io::Result<String>;
	fn exists<P: AsRef<Path>>(&self, path: P) -> bool;
}

pub struct DefaultFileProvider;

impl FileProvider for DefaultFileProvider {
	fn read_file<P: AsRef<Path>>(&self, path: P) -> io::Result<String> {
		fs::read_to_string(path)
	}

	fn exists<P: AsRef<Path>>(&self, path: P) -> bool {
		path.as_ref().exists()
	}
}