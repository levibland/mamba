pub mod environment;
pub mod evaluator;
mod stdlib;

pub use self::environment::*;
pub use self::evaluator::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
