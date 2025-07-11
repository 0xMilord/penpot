#![allow(unused_variables, dead_code, unused_mut)]

use crate::state::ShapesPool;
use super::Matrix;
use crate::uuid::Uuid;
use std::collections::HashMap;
use crate::shapes::{Shape, Path, Segment, BoolType, Type, StructureEntry};
use bezier_rs::{Bezier, BezierHandles, TValue};
use glam::DVec2;
use skia_safe as skia;

use crate::math::is_close_to;

pub trait BezierExt {
    fn to_skia_path(&self) -> skia::Path;
}

fn to_point(v: DVec2) -> skia::Point {
    skia::Point::new(v.x as f32, v.y as f32)
}

impl BezierExt for Bezier {
    fn to_skia_path(&self) -> skia::Path {
        let mut path = skia::Path::new();
        path.move_to(to_point(self.start));

        match self.handles {
            BezierHandles::Linear => {
                path.line_to(to_point(self.end));
            }
            BezierHandles::Quadratic { handle } => {
                path.cubic_to(to_point(handle), to_point(handle), to_point(self.end));
            }
            BezierHandles::Cubic {
                handle_start,
                handle_end,
            } => {
                path.cubic_to(
                    to_point(handle_start),
                    to_point(handle_end),
                    to_point(self.end),
                );
            }
        }
        path
    }
}

pub fn path_to_beziers(path: &Path) -> Vec<Bezier> {
    let mut start: Option<(f64, f64)> = None;
    let mut prev: Option<(f64, f64)> = None;

    path.segments()
        .iter()
        .filter_map(|s| match s {
            Segment::MoveTo((x, y)) => {
                let x = f64::from(*x);
                let y = f64::from(*y);
                prev = Some((x, y));
                start = Some((x, y));
                None
            }
            Segment::LineTo((x2, y2)) => {
                let (x1, y1) = prev?;
                let x2 = f64::from(*x2);
                let y2 = f64::from(*y2);
                let s = Bezier::from_linear_coordinates(x1, y1, x2, y2);
                prev = Some((x2, y2));
                Some(s)
            }
            Segment::CurveTo(((c1x, c1y), (c2x, c2y), (x2, y2))) => {
                let (x1, y1) = prev?;
                let x2 = f64::from(*x2);
                let y2 = f64::from(*y2);
                let c1x = f64::from(*c1x);
                let c1y = f64::from(*c1y);
                let c2x = f64::from(*c2x);
                let c2y = f64::from(*c2y);
                let s = Bezier::from_cubic_coordinates(x1, y1, c1x, c1y, c2x, c2y, x2, y2);
                prev = Some((x2, y2));
                Some(s)
            }
            Segment::Close => {
                let (x1, y1) = prev?;
                let (x2, y2) = start?;
                let s = Bezier::from_linear_coordinates(x1, y1, x2, y2);
                prev = Some((x2, y2));
                Some(s)
            }
        })
        .collect()
}

pub fn split_segments(path_a: &Path, path_b: &Path) -> (Vec<Bezier>, Vec<Bezier>) {
    let mut path_a = path_to_beziers(path_a);
    let mut path_b = path_to_beziers(path_b);
    let mut index_a = 0;
    let mut index_b = 0;

    loop {
        let Some(segment_a) = path_a.get(index_a) else {
            break;
        };
        let Some(segment_b) = path_b.get(index_b) else {
            break;
        };

        let intersections: Vec<f64> = segment_a
            .intersections(segment_b, Some(0.5), Some(0.05))
            .iter()
            .filter(|t| **t > 0.01 && **t < 0.99)
            .map(|t| *t)
            .collect();

        if intersections.len() > 0 {
            let segment_a = segment_a.clone();
            let segment_b = segment_b.clone();

            let t_a = intersections[0];
            let p = segment_a.evaluate(TValue::Parametric(t_a));
            let t_b = segment_b.project(p, None);

            let [a1, a2] = segment_a.split(TValue::Parametric(t_a));
            path_a.remove(index_a);
            path_a.insert(index_a, a2);
            path_a.insert(index_a, a1);

            if !is_close_to(t_b as f32, 1.0) || !is_close_to(t_b as f32, 0.0) {
                let [b1, b2] = segment_b.split(TValue::Parametric(t_b));
                path_b.remove(index_b);
                path_b.insert(index_b, b2);
                path_b.insert(index_b, b1);
            }
        } else if index_b == path_b.len() - 1 {
            index_a += 1;
            index_b = 0;
        } else {
            index_b += 1;
        }
    }

    (path_a, path_b)
}

pub fn union(
    path_a: &Path,
    segments_a: Vec<Bezier>,
    path_b: &Path,
    segments_b: Vec<Bezier>,
) -> Vec<Bezier> {
    let mut result = Vec::new();

    result.extend(
        segments_a
            .iter()
            .filter(|s| !path_b.contains(to_point(s.evaluate(TValue::Parametric(0.5))))),
    );

    result.extend(
        segments_b
            .iter()
            .filter(|s| !path_a.contains(to_point(s.evaluate(TValue::Parametric(0.5))))),
    );

    result
}

pub fn intersection(
    path_a: &Path,
    segments_a: Vec<Bezier>,
    path_b: &Path,
    segments_b: Vec<Bezier>,
) -> Vec<Bezier> {
    let mut result = Vec::new();

    result.extend(
        segments_a
            .iter()
            .filter(|s| path_b.contains(to_point(s.evaluate(TValue::Parametric(0.5))))),
    );

    result.extend(
        segments_b
            .iter()
            .filter(|s| path_a.contains(to_point(s.evaluate(TValue::Parametric(0.5))))),
    );

    result
}

pub fn difference(
    path_a: &Path,
    segments_a: Vec<Bezier>,
    path_b: &Path,
    segments_b: Vec<Bezier>,
) -> Vec<Bezier> {
    let mut result = Vec::new();

    result.extend(
        segments_a
            .iter()
            .filter(|s| !path_b.contains(to_point(s.evaluate(TValue::Parametric(0.5))))),
    );

    result.extend(
        segments_b
            .iter()
            .filter(|s| path_a.contains(to_point(s.evaluate(TValue::Parametric(0.5))))),
    );

    result
}

pub fn exclusion(
    path_a: &Path,
    segments_a: Vec<Bezier>,
    path_b: &Path,
    segments_b: Vec<Bezier>,
) -> Vec<Bezier> {
    let mut result = Vec::new();
    result.extend(segments_a.iter());
    result.extend(segments_b.iter());
    result
}

// pub fn test_bools(path_a: &Path, path_b: &Path) -> Vec<(skia::Color, Bezier)> {
//     let (segments_a, segments_b) = split_segments(path_a, path_b);
// 
//     let result = exclude(path_a, segments_a, path_b, segments_b);
// 
//     result.iter().map(|s| (skia::Color::RED, *s)).collect()
// }

fn is_close(p1: DVec2, p2: Option<DVec2>) -> bool {
    let Some(p2) = p2 else { return false; };
    super::is_close_to(p1.x as f32, p2.x as f32) &&
        super::is_close_to(p1.y as f32, p2.y as f32)
}

pub fn beziers_to_segments(beziers: &Vec<Bezier>) -> Vec<Segment> {
    let mut result = Vec::new();
    let mut first: Option<DVec2> = None;
    let mut prev: Option<DVec2> = None;

    for bezier in beziers.iter() {
        if first.is_none() {
            first = Some(bezier.start);
        }

        if prev.is_none() {
            result.push(Segment::MoveTo((bezier.start.x as f32, bezier.start.y as f32)));
        }
        
        match bezier.handles {
            BezierHandles::Linear => {
                result.push(Segment::LineTo((bezier.end.x as f32, bezier.end.y as f32)));
            }
            BezierHandles::Quadratic { handle } => {
                result.push(Segment::CurveTo((
                    (handle.x as f32, handle.y as f32),
                    (handle.x as f32, handle.y as f32),
                    (bezier.end.x as f32, bezier.end.y as f32)
                )));
            }
            BezierHandles::Cubic { handle_start, handle_end} => {
                result.push(Segment::CurveTo((
                    (handle_start.x as f32, handle_start.y as f32),
                    (handle_end.x as f32, handle_end.y as f32),
                    (bezier.end.x as f32, bezier.end.y as f32)
                )));
            }
        }

        if is_close(bezier.end, prev) {
            result.push(Segment::Close);
            prev = None;
        } else {
            prev = Some(bezier.end);
        }
    }

    result
}

pub fn update_bool_to_path(
    shape: &Shape,
    shapes: &ShapesPool,
    modifiers: &HashMap<Uuid, Matrix>,
    structure: &HashMap<Uuid, Vec<StructureEntry>>,
) -> Option<Shape> {

    let children_ids =
        shape.modified_children_ids(structure.get(&shape.id), true);

    let shape_a = shapes.get(&children_ids[1])?;
    let shape_b = shapes.get(&children_ids[0])?;

    let shape_a = &mut shape_a.clone();
    let shape_b = &mut shape_b.clone();

    if let Some(shape_modifiers) = modifiers.get(&shape_a.id) {
        shape_a.apply_transform(shape_modifiers);
    }

    if let Some(shape_modifiers) = modifiers.get(&shape_b.id) {
        shape_b.apply_transform(shape_modifiers);
    }
    
    let Type::Path(path_a) = &shape_a.shape_type else { return None; };
    let Type::Path(path_b) = &shape_b.shape_type else { return None; };
    
    let (segments_a, segments_b) = split_segments(path_a, path_b);

    let mut shape = shape.clone();
    let Type::Bool(bool_data) = &mut shape.shape_type else { return None; };

    let beziers = match bool_data.bool_type {
        BoolType::Union => union(path_a, segments_a, path_b, segments_b),
        BoolType::Difference => difference(path_a, segments_a, path_b, segments_b),
        BoolType::Intersection => intersection(path_a, segments_a, path_b, segments_b),
        BoolType::Exclusion => exclusion(path_a, segments_a, path_b, segments_b),
    };

    bool_data.path = Path::new(beziers_to_segments(&beziers));
    Some(shape)
}
