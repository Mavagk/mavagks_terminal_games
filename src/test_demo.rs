use crate::game::Game;

pub struct TestDemo {

}

impl Game for TestDemo {
	fn draw(&mut self, _writer: &mut std::io::Stdout) {}
	
	fn event(&mut self, _event: &crossterm::event::Event) {}
	
	fn click(&mut self, _pos: (u16, u16), _button: crossterm::event::MouseButton) {}
}

impl TestDemo {
	pub fn new() -> Self {
		TestDemo {
			
		}
	}
}