use bevy::prelude::*;
use bevy::render::texture::{Extent3d, TextureDimension, TextureFormat};
use rand::seq::SliceRandom;
use rand::Rng;
use std::cell::RefCell;
use std::ops::DerefMut;

static WINDOW_SIZE: f32 = 1000.0;
static BOARD_SIZE: usize = 100;
static BOARD_SIZE_F32: f32 = BOARD_SIZE as f32;

thread_local! {
    // unstable library feature 'thread_id_value', tracking issue #67939
    static RNG: RefCell<rand::rngs::SmallRng> = RefCell::new(rand::SeedableRng::seed_from_u64(0 /*std::thread::current().id().as_u64()*/ ));
}

fn main() {
    App::build()
        .insert_resource(WindowDescriptor {
            title: "Game".to_string(),
            width: WINDOW_SIZE,
            height: WINDOW_SIZE,
            vsync: true,
            resizable: false,
            ..Default::default()
        })
        .add_plugins(DefaultPlugins)
        .add_plugin(PowderPlugin)
        .add_plugin(bevy::diagnostic::FrameTimeDiagnosticsPlugin::default())
        .add_plugin(bevy::diagnostic::LogDiagnosticsPlugin::default())
        .run();
}

#[derive(Default)]
pub struct PowderPlugin;

#[derive(Clone, PartialEq, Eq)]
pub enum Material {
    NONE,
    SAND,
    OXYGEN,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Particle {
    pub material: Material,
    pub index: usize,
}

#[derive(Clone)]
pub struct PowderData {
    pub texture: Handle<Texture>,
    pub particles: Vec<Particle>,
    pub index_particle_id_map: Vec<Option<usize>>,
}

impl Default for PowderData {
    fn default() -> Self {
        let mut particles = Vec::new();
        let mut index_particle_id_map = Vec::new();

        for i in 0..BOARD_SIZE * BOARD_SIZE / 4 {
            if i % 7 == 0 {
                let index = index_particle_id_map.len();
                index_particle_id_map.push(Some(particles.len()));
                particles.push(Particle {
                    material: Material::OXYGEN,
                    index,
                });
            } else {
                index_particle_id_map.push(None);
            }
        }
        for _ in 0..BOARD_SIZE * BOARD_SIZE / 4 {
            let index = index_particle_id_map.len();
            index_particle_id_map.push(Some(particles.len()));
            particles.push(Particle {
                material: Material::SAND,
                index,
            });
        }
        for i in 0..BOARD_SIZE * BOARD_SIZE / 2 {
            if i % 7 == 0 {
                let index = index_particle_id_map.len();
                index_particle_id_map.push(Some(particles.len()));
                particles.push(Particle {
                    material: Material::OXYGEN,
                    index,
                });
            } else {
                index_particle_id_map.push(None);
            }
        }

        debug_assert_eq!(index_particle_id_map.len(), BOARD_SIZE * BOARD_SIZE);

        Self {
            particles,
            index_particle_id_map,
            texture: Default::default(),
        }
    }
}

fn simulate(mut powder_data: ResMut<PowderData>, mut textures: ResMut<Assets<Texture>>) {
    let len = powder_data.index_particle_id_map.len();
    let mut order: Vec<_> = (0..powder_data.particles.len()).collect();
    random_shuffle(&mut order);
    let order = order;

    // Update particles
    for &particle_id in &order {
        let particle = powder_data.particles[particle_id].clone();
        let neighbor_index_above = if particle.index / BOARD_SIZE > 0 {
            Some(particle.index - BOARD_SIZE)
        } else {
            None
        };
        let neighbor_index_below = if particle.index / BOARD_SIZE < BOARD_SIZE - 1 {
            Some(particle.index + BOARD_SIZE)
        } else {
            None
        };
        let neighbor_index_left = if particle.index % BOARD_SIZE > 0 {
            Some(particle.index - 1)
        } else {
            None
        };
        let neighbor_index_right = if particle.index % BOARD_SIZE < BOARD_SIZE - 1 {
            Some(particle.index + 1)
        } else {
            None
        };
        let neighbor_index_left_below = if neighbor_index_left.is_some() && neighbor_index_below.is_some() {
            Some(particle.index - 1 + BOARD_SIZE)
        } else {
            None
        };
        let neighbor_index_right_below = if neighbor_index_right.is_some() && neighbor_index_below.is_some() {
            Some(particle.index + 1 + BOARD_SIZE)
        } else {
            None
        };

        let mut neighbor_indexes: Vec<usize> = Vec::new();
        neighbor_indexes.extend(neighbor_index_above.iter());
        neighbor_indexes.extend(neighbor_index_below.iter());
        neighbor_indexes.extend(neighbor_index_left.iter());
        neighbor_indexes.extend(neighbor_index_right.iter());
        debug_assert!(
            neighbor_indexes.iter().all(|&neighbor| neighbor < len),
            "{:?}",
            neighbor_indexes
        );
        let neighbor_indexes = neighbor_indexes;

        let mut slide_neighbor_indexes: Vec<usize> = Vec::new();
        slide_neighbor_indexes.extend(neighbor_index_left_below.iter());
        slide_neighbor_indexes.extend(neighbor_index_right_below.iter());
        let slide_neighbor_indexes = slide_neighbor_indexes;

        if random_bool(particle.material.dissipation_probability()) {
            // Dissipation
            if let Some(&neighbor_index) = random_choice(&neighbor_indexes) {
                let neighbor_particle_id = powder_data.index_particle_id_map[neighbor_index];
                if neighbor_particle_id
                    .map(|id| powder_data.particles[id].material.is_gas())
                    .unwrap_or(true)
                {
                    let PowderData {
                        particles,
                        index_particle_id_map,
                        ..
                    } = &mut *powder_data;
                    swap_indices(
                        particles,
                        index_particle_id_map,
                        particle.index,
                        neighbor_index,
                    );
                }
            }
        } else if random_bool(particle.material.gravity_probability()) {
            // Gravity
            if let Some(&neighbor_index_below) = neighbor_index_below.as_ref() {
                let neighbor_particle_id = powder_data.index_particle_id_map[neighbor_index_below];
                if neighbor_particle_id
                    .map(|id| powder_data.particles[id].material.is_gas())
                    .unwrap_or(true)
                {
                    let PowderData {
                        particles,
                        index_particle_id_map,
                        ..
                    } = &mut *powder_data;
                    swap_indices(
                        particles,
                        index_particle_id_map,
                        particle.index,
                        neighbor_index_below,
                    );
                }
            }
        } else if random_bool(particle.material.slide_probability()) {
            // Slide
            if let Some(&slide_neighbor_index) = random_choice(&slide_neighbor_indexes) {
                let slide_neighbor_particle_id = powder_data.index_particle_id_map[slide_neighbor_index];
                if slide_neighbor_particle_id.map(|id| powder_data.particles[id].material.is_gas()).unwrap_or(true) {
                    let PowderData {
                        particles,
                        index_particle_id_map,
                        ..
                    } = &mut *powder_data;
                    swap_indices(
                        particles,
                        index_particle_id_map,
                        particle.index,
                        slide_neighbor_index,
                    );
                }
            }
        }
    }

    #[cfg(debug)]
    powder_data.verify();

    // Render
    let texture = textures.get_mut(powder_data.texture.clone()).unwrap();
    for index in 0..len {
        set_pixel_color_index(texture, index, &Material::NONE.color());
    }

    for particle in &powder_data.particles {
        set_pixel_color_index(texture, particle.index, &particle.material.color());
    }
}

fn set_pixel_rgb_index(texture: &mut Texture, index: usize, r: u8, g: u8, b: u8) {
    debug_assert_eq!(texture.format, TextureFormat::Rgba8UnormSrgb);
    // let index = 4 * (x + y * texture.size.width as usize);
    let index = index * 4;
    texture.data[index] = r;
    texture.data[index + 1] = g;
    texture.data[index + 2] = b;
}

fn set_pixel_color_index(texture: &mut Texture, index: usize, color: &Color) {
    set_pixel_rgb_index(texture, index, color.r, color.g, color.b)
}

fn random_bool(probability: f64) -> bool {
    RNG.with(|rng| rng.borrow_mut().gen_bool(probability))
}

fn random_choice<T>(slice: &[T]) -> Option<&T> {
    RNG.with(|rng| slice.choose(rng.borrow_mut().deref_mut()))
}

fn random_shuffle<T>(slice: &mut [T]) {
    RNG.with(|rng| slice.shuffle(rng.borrow_mut().deref_mut()))
}

fn swap_indices(
    particles: &mut Vec<Particle>,
    index_particle_id_map: &mut Vec<Option<usize>>,
    mut i: usize,
    mut j: usize,
) {
    debug_assert_ne!(i, j);

    if index_particle_id_map[i].is_some() && index_particle_id_map[j].is_some() {
        debug_assert_ne!(index_particle_id_map[i], index_particle_id_map[j]);
        index_particle_id_map.swap(i, j);
        let index_i = particles[index_particle_id_map[i].unwrap()].index;
        let index_j = particles[index_particle_id_map[j].unwrap()].index;
        particles[index_particle_id_map[i].unwrap()].index = index_j;
        particles[index_particle_id_map[j].unwrap()].index = index_i;
    } else if index_particle_id_map[i].is_some() || index_particle_id_map[j].is_some() {
        if index_particle_id_map[i].is_none() {
            std::mem::swap(&mut i, &mut j);
        }
        // now i is Some and j is None

        particles[index_particle_id_map[i].unwrap()].index = j;
        index_particle_id_map[j] = index_particle_id_map[i];
        index_particle_id_map[i] = None;
    }
}

impl Material {
    fn color(&self) -> Color {
        match self {
            Material::NONE => Color::new(0, 0, 0),
            Material::SAND => Color::new(251, 252, 210),
            Material::OXYGEN => Color::new(232, 248, 255),
        }
    }

    fn gravity_probability(&self) -> f64 {
        match self {
            Material::NONE => 0.0,
            Material::SAND => 0.2,
            Material::OXYGEN => 1e-3,
        }
    }

    fn dissipation_probability(&self) -> f64 {
        match self {
            Material::NONE => 0.0,
            Material::SAND => 0.0,
            Material::OXYGEN => 0.9,
        }
    }

    fn slide_probability(&self) -> f64 {
        match self {
            Material::NONE => 0.0,
            Material::SAND => 0.2,
            Material::OXYGEN => 0.0,
        }
    }

    fn is_solid(&self) -> bool {
        match self {
            Material::NONE => false,
            Material::SAND => true,
            Material::OXYGEN => false,
        }
    }

    fn is_liquid(&self) -> bool {
        match self {
            Material::NONE => false,
            Material::SAND => false,
            Material::OXYGEN => false,
        }
    }

    fn is_gas(&self) -> bool {
        match self {
            Material::NONE => false,
            Material::SAND => false,
            Material::OXYGEN => true,
        }
    }
}

impl Plugin for PowderPlugin {
    fn build(&self, app: &mut AppBuilder) {
        app.insert_resource(PowderData::default());
        app.add_startup_system(powder_plugin_setup.system());
        app.add_system(simulate.system());
    }
}

fn powder_plugin_setup(
    mut commands: Commands,
    mut textures: ResMut<Assets<Texture>>,
    mut powder_data: ResMut<PowderData>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    powder_data.texture = textures.add(Texture::new_fill(
        Extent3d::new(BOARD_SIZE as u32, BOARD_SIZE as u32, 1),
        TextureDimension::D2,
        &[0, 0, 0, 255],
        TextureFormat::Rgba8UnormSrgb,
    ));

    println!(
        "Texture data len is {}",
        textures
            .get(powder_data.texture.clone())
            .unwrap()
            .data
            .len()
    );

    commands.spawn_bundle(OrthographicCameraBundle::new_2d());
    commands.spawn_bundle(SpriteBundle {
        material: materials.add(powder_data.texture.clone().into()),
        transform: Transform::from_scale(Vec3::new(
            WINDOW_SIZE / BOARD_SIZE_F32,
            WINDOW_SIZE / BOARD_SIZE_F32,
            0.0,
        )),
        ..Default::default()
    });
}

struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Color {
    fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }
}

impl PowderData {
    fn verify(&self) {
        for (index, id) in self.index_particle_id_map.iter().enumerate() {
            if let Some(&id) = id.as_ref() {
                debug_assert_eq!(index, self.particles[id].index);
            }
        }

        for (id, particle) in self.particles.iter().enumerate() {
            debug_assert_eq!(Some(id), self.index_particle_id_map[particle.index]);
        }
    }
}
