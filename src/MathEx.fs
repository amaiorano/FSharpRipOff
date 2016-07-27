module MathEx

open System

let min (x : float) (y : float) = Math.Min(x, y)
let max (x : float) (y : float) = Math.Max(x, y)

let clamp minV maxV v = 
    v
    |> max minV
    |> min maxV

let cos = Math.Cos
let sin = Math.Sin
let atan2 = Math.Atan2
let cossin rads = (cos (rads), sin (rads))
let twoPI = 2. * Math.PI
let normalizeAngle2PI rads = Math.IEEERemainder(rads, twoPI)

let normalizeAnglePI rads = 
    match (normalizeAngle2PI rads) with
    | x when x <= Math.PI -> x
    | x -> twoPI - x

// Returns intersection of lines ps1,pe1 and ps2,pe2
let intersection (ps1x, ps1y) (pe1x, pe1y) (ps2x, ps2y) (pe2x, pe2y) = 
    let A1 = pe1y - ps1y
    let B1 = ps1x - pe1x
    let C1 = A1 * ps1x + B1 * ps1y
    let A2 = pe2y - ps2y
    let B2 = ps2x - pe2x
    let C2 = A2 * ps2x + B2 * ps2y
    let delta = A1 * B2 - A2 * B1
    //    if delta = 0. then None
    assert (delta <> 0.)
    (B2 * C1 - B1 * C2) / delta, (A1 * C2 - A2 * C1) / delta

type Approach = 
    // Approaches current towards target using a viscous damped approach; by default, it will reach
    // 99% (factor) of target within 1 (timeToTarget) second, then will keep getting closer but never
    // reach the (asymptotic) target. The tolerance can be set to make sure the target is reached, but
    // keep in mind that this modifies the shape of the curve (slightly).
    static member damped (current, target, dt, ?factor, ?timeToTarget, ?tolerance) = 
        let factor = defaultArg factor 0.99
        let timeToTarget = defaultArg timeToTarget 1.
        let tolerance = defaultArg tolerance 0.
        if current = target || timeToTarget = 0. then target
        else 
            let delta = target - current
            let deltaSign = float (Math.Sign(float32 delta))
            let newTarget = target + deltaSign * tolerance
            let alpha = 1. - Math.Pow(1. - factor, dt / timeToTarget)
            let step = (newTarget - current) * alpha
            if (step * deltaSign) > (delta * deltaSign) then target
            else current + step