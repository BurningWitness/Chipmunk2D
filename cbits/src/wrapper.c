#include "chipmunk/chipmunk.h"
#include "wrapper.h"

// start of cpVect.h

void w_cpvzero(cpVect *out)
{
  *out = cpvzero;
}

void w_cpv(const cpFloat x, const cpFloat y, cpVect *out)
{
  *out = cpv (x, y);
}

cpBool w_cpveql(const cpVect *v1, const cpVect *v2)
{
  return cpveql (*v1, *v2);
}

void w_cpvadd(const cpVect *v1, const cpVect *v2, cpVect *out)
{
  *out = cpvadd (*v1, *v2);
}

void w_cpvsub(const cpVect *v1, const cpVect *v2, cpVect *out)
{
  *out = cpvsub (*v1, *v2);
}

void w_cpvneg(const cpVect *v, cpVect *out)
{
  *out = cpvneg (*v);
}

void w_cpvmult(const cpVect *v, const cpFloat s, cpVect *out)
{
  *out = cpvmult (*v, s);
}

cpFloat w_cpvdot(const cpVect *v1, const cpVect *v2)
{
  return cpvdot (*v1, *v2);
}

cpFloat w_cpvcross(const cpVect *v1, const cpVect *v2)
{
  return cpvcross (*v1, *v2);
}

void w_cpvperp(const cpVect *v, cpVect *out)
{
  *out = cpvperp (*v);
}

void w_cpvrperp(const cpVect *v, cpVect *out)
{
  *out = cpvrperp (*v);
}

void w_cpvproject(const cpVect *v1, const cpVect *v2, cpVect *out)
{
  *out = cpvproject (*v1, *v2);
}

void w_cpvforangle(const cpFloat a, cpVect *out)
{
  *out = cpvforangle (a);
}

cpFloat w_cpvtoangle(const cpVect *v)
{
  return cpvtoangle (*v);
}

void w_cpvrotate(const cpVect *v1, const cpVect *v2, cpVect *out)
{
  *out = cpvrotate (*v1, *v2);
}

void w_cpvunrotate(const cpVect *v1, const cpVect *v2, cpVect *out)
{
  *out = cpvunrotate (*v1, *v2);
}

cpFloat w_cpvlengthsq(const cpVect *v)
{
  return cpvlengthsq (*v);
}

cpFloat w_cpvlength(const cpVect *v)
{
  return cpvlength (*v);
}

void w_cpvlerp(const cpVect *v1, const cpVect *v2, const cpFloat t, cpVect *out)
{
  *out = cpvlerp (*v1, *v2, t);
}

void w_cpvnormalize(const cpVect *v, cpVect *out)
{
  *out = cpvnormalize (*v);
}

void w_cpvslerp(const cpVect *v1, const cpVect *v2, const cpFloat t, cpVect *out)
{
  *out = cpvslerp (*v1, *v2, t);
}

void w_cpvslerpconst(const cpVect *v1, const cpVect *v2, const cpFloat a, cpVect *out)
{
  *out = cpvslerpconst (*v1, *v2, a);
}

void w_cpvclamp(const cpVect *v, const cpFloat len, cpVect *out)
{
  *out = cpvclamp (*v, len);
}

void w_cpvlerpconst(cpVect *v1, cpVect *v2, cpFloat d, cpVect *out)
{
  *out = cpvlerpconst (*v1, *v2, d);
}

cpFloat w_cpvdist(const cpVect *v1, const cpVect *v2)
{
  return cpvdist (*v1, *v2);
}

cpFloat w_cpvdistsq(const cpVect *v1, const cpVect *v2)
{
  return cpvdistsq (*v1, *v2);
}

cpBool w_cpvnear(const cpVect *v1, const cpVect *v2, const cpFloat dist)
{
  return cpvnear (*v1, *v2, dist);
}

void w_cpMat2x2Transform(cpMat2x2 *m, cpVect *v, cpVect *out)
{
  *out = cpMat2x2Transform (*m, *v);
}

// end of cpVect.h

// start of cpBB.h

void w_cpBBNew(const cpFloat l, const cpFloat b, const cpFloat r, const cpFloat t, cpBB *out)
{
  *out = cpBBNew (l, b, r, t);
}

void w_cpBBNewForExtents(const cpVect *c, const cpFloat hw, const cpFloat hh, cpBB *out)
{
  *out = cpBBNewForExtents (*c, hw, hh);
}

void w_cpBBNewForCircle(const cpVect *p, const cpFloat r, cpBB *out)
{
  *out = cpBBNewForCircle (*p, r);
}

cpBool w_cpBBIntersects(const cpBB *a, const cpBB *b)
{
  return cpBBIntersects (*a, *b);
}

cpBool w_cpBBContainsBB(const cpBB *bb, const cpBB *other)
{
  return cpBBContainsBB (*bb, *other);
}

cpBool w_cpBBContainsVect(const cpBB *bb, const cpVect *v)
{
  return cpBBContainsVect (*bb, *v);
}

void w_cpBBMerge(const cpBB *a, const cpBB *b, cpBB *out)
{
  *out = cpBBMerge (*a, *b);
}

void w_cpBBExpand(const cpBB *bb, const cpVect *v, cpBB *out)
{
  *out = cpBBExpand (*bb, *v);
}

void w_cpBBCenter(cpBB *bb, cpVect *out)
{
  *out = cpBBCenter (*bb);
}

cpFloat w_cpBBArea(cpBB *bb)
{
  return cpBBArea (*bb);
}

cpFloat w_cpBBMergedArea(cpBB *a, cpBB *b)
{
  return cpBBMergedArea (*a, *b);
}

cpFloat w_cpBBSegmentQuery(cpBB *bb, cpVect *a, cpVect *b)
{
  return cpBBSegmentQuery (*bb, *a, *b);
}

cpBool w_cpBBIntersectsSegment(cpBB *bb, cpVect *a, cpVect *b)
{
  return cpBBIntersectsSegment (*bb, *a, *b);
}

void w_cpBBClampVect(const cpBB *bb, const cpVect *v, cpVect *out)
{
  *out = cpBBClampVect (*bb, *v);
}

void w_cpBBWrapVect(const cpBB *bb, const cpVect *v, cpVect *out)
{
  *out = cpBBWrapVect (*bb, *v);
}

void w_cpBBOffset(const cpBB *bb, const cpVect *v, cpBB *out)
{
  *out = cpBBOffset (*bb, *v);
}

// end of cpBB.h

// start of cpTransform.h

void w_cpTransformIdentity (cpTransform *out)
{
  *out = cpTransformIdentity;
}

void w_cpTransformNew(cpFloat a, cpFloat b, cpFloat c, cpFloat d, cpFloat tx, cpFloat ty, cpTransform *out)
{
  *out = cpTransformNew (a, b, c, d, tx, ty);
}

void w_cpTransformNewTranspose(cpFloat a, cpFloat c, cpFloat tx, cpFloat b, cpFloat d, cpFloat ty, cpTransform *out)
{
  *out = cpTransformNewTranspose (a, b, c, d, tx, ty);
}

void w_cpTransformInverse(cpTransform *t, cpTransform *out)
{
  *out = cpTransformInverse (*t);
}

void w_cpTransformMult(cpTransform *t1, cpTransform *t2, cpTransform *out)
{
  *out = cpTransformMult (*t1, *t2);
}

void w_cpTransformPoint(cpTransform *t, cpVect *p, cpVect *out)
{
  *out = cpTransformPoint (*t, *p);
}

void w_cpTransformVect(cpTransform *t, cpVect *v, cpVect *out)
{
  *out = cpTransformVect (*t, *v);
}

void w_cpTransformbBB(cpTransform *t, cpBB *bb, cpBB *out)
{
  *out = cpTransformbBB (*t, *bb);
}

void w_cpTransformTranslate(cpVect *translate, cpTransform *out)
{
  *out = cpTransformTranslate (*translate);
}

void w_cpTransformScale(cpFloat scaleX, cpFloat scaleY, cpTransform *out)
{
  *out = cpTransformScale (scaleX, scaleY);
}

void w_cpTransformRotate(cpFloat radians, cpTransform *out)
{
  *out = cpTransformRotate (radians);
}

void w_cpTransformRigid(cpVect *translate, cpFloat radians, cpTransform *out)
{
  *out = cpTransformRigid (*translate, radians);
}

void w_cpTransformRigidInverse(cpTransform *t, cpTransform *out)
{
  *out = cpTransformRigidInverse (*t);
}

void w_cpTransformWrap(cpTransform *outer, cpTransform *inner, cpTransform *out)
{
  *out = cpTransformWrap (*outer, *inner);
}

void w_cpTransformWrapInverse(cpTransform *outer, cpTransform *inner, cpTransform *out)
{
  *out = cpTransformWrapInverse (*outer, *inner);
}

void w_cpTransformOrtho(cpBB *bb, cpTransform *out)
{
  *out = cpTransformOrtho (*bb);
}

void w_cpTransformBoneScale(cpVect *v0, cpVect *v1, cpTransform *out)
{
  *out = cpTransformBoneScale (*v0, *v1);
}

void w_cpTransformAxialScale(cpVect *axis, cpVect *pivot, cpFloat scale, cpTransform *out)
{
  *out = cpTransformAxialScale (*axis, *pivot, scale);
}

// end of cpTransform.h

// start of cpSpatialIndex.h

void w_cpSpatialIndexQuery(cpSpatialIndex *index, void *obj, cpBB *bb, cpSpatialIndexQueryFunc func, void *data)
{
  return cpSpatialIndexQuery (index, obj, *bb, func, data);
}

void w_cpSpatialIndexSegmentQuery(cpSpatialIndex *index, void *obj, cpVect *a, cpVect *b, cpFloat t_exit, cpSpatialIndexSegmentQueryFunc func, void *data)
{
  return cpSpatialIndexSegmentQuery (index, obj, *a, *b, t_exit, func, data);
}

// end of cpSpatialIndex.h

// start of cpArbiter.h

void w_cpArbiterGetContactPointSet(const cpArbiter *arb, cpContactPointSet *out)
{
  *out = cpArbiterGetContactPointSet (arb);
}

void w_cpArbiterGetSurfaceVelocity(cpArbiter *arb, cpVect *out)
{
  *out = cpArbiterGetSurfaceVelocity (arb);
}

void w_cpArbiterSetSurfaceVelocity(cpArbiter *arb, cpVect *vr)
{
  return cpArbiterSetSurfaceVelocity (arb, *vr);
}

void w_cpArbiterTotalImpulse(const cpArbiter *arb, cpVect *out)
{
  *out = cpArbiterTotalImpulse (arb);
}

void w_cpArbiterGetNormal(const cpArbiter *arb, cpVect *out)
{
  *out = cpArbiterGetNormal (arb);
}

void w_cpArbiterGetPointA(const cpArbiter *arb, int i, cpVect *out)
{
  *out = cpArbiterGetPointA (arb, i);
}

void w_cpArbiterGetPointB(const cpArbiter *arb, int i, cpVect *out)
{
  *out = cpArbiterGetPointB (arb, i);
}

// end of cpArbiter.h

// start of cpBody.h

void w_cpBodyGetPosition(const cpBody *body, cpVect *out)
{
  *out = cpBodyGetPosition (body);
}

void w_cpBodySetPosition(cpBody *body, cpVect *pos)
{
  return cpBodySetPosition (body, *pos);
}

void w_cpBodyGetCenterOfGravity(const cpBody *body, cpVect *out)
{
  *out = cpBodyGetCenterOfGravity (body);
}

void w_cpBodySetCenterOfGravity(cpBody *body, cpVect* cog)
{
  return cpBodySetCenterOfGravity (body, *cog);
}

void w_cpBodyGetVelocity(const cpBody *body, cpVect *out)
{
  *out = cpBodyGetVelocity (body);
}

void w_cpBodySetVelocity(cpBody *body, cpVect *velocity)
{
  return cpBodySetVelocity (body, *velocity);
}

void w_cpBodyGetForce(const cpBody *body, cpVect *out)
{
  *out = cpBodyGetForce (body);
}

void w_cpBodySetForce(cpBody *body, cpVect *force)
{
  return cpBodySetForce (body, *force);
}

void w_cpBodyGetRotation(const cpBody *body, cpVect *out)
{
  *out = cpBodyGetRotation (body);
}

void w_cpBodyUpdateVelocity(cpBody *body, cpVect *gravity, cpFloat damping, cpFloat dt)
{
  return cpBodyUpdateVelocity (body, *gravity, damping, dt);
}

void w_cpBodyLocalToWorld(const cpBody *body, const cpVect *point, cpVect *out)
{
  *out = cpBodyLocalToWorld (body, *point);
}

void w_cpBodyWorldToLocal(const cpBody *body, const cpVect *point, cpVect *out)
{
  *out = cpBodyWorldToLocal (body, *point);
}

void w_cpBodyApplyForceAtWorldPoint(cpBody *body, cpVect *force, cpVect *point)
{
  return cpBodyApplyForceAtWorldPoint (body, *force, *point);
}

void w_cpBodyApplyForceAtLocalPoint(cpBody *body, cpVect *force, cpVect *point)
{
  return cpBodyApplyForceAtLocalPoint (body, *force, *point);
}

void w_cpBodyApplyImpulseAtWorldPoint(cpBody *body, cpVect *impulse, cpVect *point)
{
  return cpBodyApplyImpulseAtWorldPoint (body, *impulse, *point);
}

void w_cpBodyApplyImpulseAtLocalPoint(cpBody *body, cpVect *impulse, cpVect *point)
{
  return cpBodyApplyImpulseAtLocalPoint (body, *impulse, *point);
}

void w_cpBodyGetVelocityAtWorldPoint(const cpBody *body, cpVect *point, cpVect *out)
{
  *out = cpBodyGetVelocityAtWorldPoint (body, *point);
}

void w_cpBodyGetVelocityAtLocalPoint(const cpBody *body, cpVect *point, cpVect *out)
{
  *out = cpBodyGetVelocityAtLocalPoint (body, *point);
}

// end of cpBody.h

// start of cpShape.h

void w_cp_shape_filter_all (cpShapeFilter *out)
{
  *out = CP_SHAPE_FILTER_ALL;
}

void w_cp_shape_filter_none (cpShapeFilter *out)
{
  *out = CP_SHAPE_FILTER_NONE;
}

void w_cpShapeCacheBB(cpShape *shape, cpBB *out)
{
  *out = cpShapeCacheBB (shape);
}

void w_cpShapeUpdate(cpShape *shape, cpTransform *transform, cpBB *out)
{
  *out = cpShapeUpdate (shape, *transform);
}

cpFloat w_cpShapePointQuery(const cpShape *shape, cpVect *p, cpPointQueryInfo *out)
{
  return cpShapePointQuery (shape, *p, out);
}

cpBool w_cpShapeSegmentQuery(const cpShape *shape, cpVect *a, cpVect *b, cpFloat radius, cpSegmentQueryInfo *info)
{
  return cpShapeSegmentQuery (shape, *a, *b, radius, info);
}

void w_cpShapesCollide(const cpShape *a, const cpShape *b, cpContactPointSet *out)
{
  *out = cpShapesCollide (a, b);
}

void w_cpShapeGetCenterOfGravity(cpShape *shape, cpVect *out)
{
  *out = cpShapeGetCenterOfGravity (shape);
}

void w_cpShapeGetBB(const cpShape *shape, cpBB *out)
{
  *out = cpShapeGetBB (shape);
}

void w_cpShapeGetSurfaceVelocity(const cpShape *shape, cpVect *out)
{
  *out = cpShapeGetSurfaceVelocity (shape);
}

void w_cpShapeSetSurfaceVelocity(cpShape *shape, cpVect *surfaceVelocity)
{
  return cpShapeSetSurfaceVelocity (shape, *surfaceVelocity);
}

void w_cpShapeGetFilter(const cpShape *shape, cpShapeFilter *out)
{
  *out = cpShapeGetFilter (shape);
}

void w_cpShapeSetFilter(cpShape *shape, cpShapeFilter *filter)
{
  return cpShapeSetFilter (shape, *filter);
}

cpCircleShape* w_cpCircleShapeInit(cpCircleShape *circle, cpBody *body, cpFloat radius, cpVect *offset)
{
  return cpCircleShapeInit (circle, body, radius, *offset);
}

cpShape* w_cpCircleShapeNew(cpBody *body, cpFloat radius, cpVect *offset)
{
  return cpCircleShapeNew (body, radius, *offset);
}

void w_cpCircleShapeGetOffset(const cpShape *shape, cpVect *out)
{
  *out = cpCircleShapeGetOffset (shape);
}

cpSegmentShape* w_cpSegmentShapeInit(cpSegmentShape *seg, cpBody *body, cpVect *a, cpVect *b, cpFloat radius)
{
  return cpSegmentShapeInit (seg, body, *a, *b, radius);
}

cpShape* w_cpSegmentShapeNew(cpBody *body, cpVect *a, cpVect *b, cpFloat radius)
{
  return cpSegmentShapeNew (body, *a, *b, radius);
}

void w_cpSegmentShapeSetNeighbors(cpShape *shape, cpVect *prev, cpVect *next)
{
  return cpSegmentShapeSetNeighbors (shape, *prev, *next);
}

void w_cpSegmentShapeGetA(const cpShape *shape, cpVect *out)
{
  *out = cpSegmentShapeGetA (shape);
}

void w_cpSegmentShapeGetB(const cpShape *shape, cpVect *out)
{
  *out = cpSegmentShapeGetB (shape);
}

void w_cpSegmentShapeGetNormal(const cpShape *shape, cpVect *out)
{
  *out = cpSegmentShapeGetNormal (shape);
}

// end of cpShape.h

// start of cpPolyShape.h

cpPolyShape* w_cpPolyShapeInit(cpPolyShape *poly, cpBody *body, int count, const cpVect *verts, cpTransform *transform, cpFloat radius)
{
  return cpPolyShapeInit (poly, body, count, verts, *transform, radius);
}

cpShape* w_cpPolyShapeNew(cpBody *body, int count, const cpVect *verts, cpTransform *transform, cpFloat radius)
{
  return cpPolyShapeNew (body, count, verts, *transform, radius);
}

cpPolyShape* w_cpBoxShapeInit2(cpPolyShape *poly, cpBody *body, cpBB *box, cpFloat radius)
{
  return cpBoxShapeInit2 (poly, body, *box, radius);
}

cpShape* w_cpBoxShapeNew2(cpBody *body, cpBB *box, cpFloat radius)
{
  return cpBoxShapeNew2 (body, *box, radius);
}

void w_cpPolyShapeGetVert(const cpShape *shape, int index, cpVect *out)
{
  *out = cpPolyShapeGetVert (shape, index);
}

// end of cpPolyShape.h

// start of cpConstraint.h

// start of cpPinJoint.h

cpPinJoint* w_cpPinJointInit(cpPinJoint *joint, cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB)
{
  return cpPinJointInit (joint, a, b, *anchorA, *anchorB);
}

cpConstraint* w_cpPinJointNew(cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB)
{
  return cpPinJointNew (a, b, *anchorA, *anchorB);
}

void w_cpPinJointGetAnchorA(const cpConstraint *constraint, cpVect *out)
{
  *out = cpPinJointGetAnchorA (constraint);
}

void w_cpPinJointSetAnchorA(cpConstraint *constraint, cpVect *anchorA)
{
  return cpPinJointSetAnchorA (constraint, *anchorA);
}

void w_cpPinJointGetAnchorB(const cpConstraint *constraint, cpVect *out)
{
  *out = cpPinJointGetAnchorB (constraint);
}

void w_cpPinJointSetAnchorB(cpConstraint *constraint, cpVect *anchorB)
{
  return cpPinJointSetAnchorB (constraint, *anchorB);
}

// end of cpPinJoint.h

// start of cpSlideJoint.h

cpSlideJoint* w_cpSlideJointInit(cpSlideJoint *joint, cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB, cpFloat min, cpFloat max)
{
  return cpSlideJointInit (joint, a, b, *anchorA, *anchorB, min, max);
}

cpConstraint* w_cpSlideJointNew(cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB, cpFloat min, cpFloat max)
{
  return cpSlideJointNew (a, b, *anchorA, *anchorB, min, max);
}

void w_cpSlideJointGetAnchorA(const cpConstraint *constraint, cpVect *out)
{
  *out = cpSlideJointGetAnchorA (constraint);
}

void w_cpSlideJointSetAnchorA(cpConstraint *constraint, cpVect *anchorA)
{
  return cpSlideJointSetAnchorA (constraint, *anchorA);
}

void w_cpSlideJointGetAnchorB(const cpConstraint *constraint, cpVect *out)
{
  *out = cpSlideJointGetAnchorB (constraint);
}

void w_cpSlideJointSetAnchorB(cpConstraint *constraint, cpVect *anchorB)
{
  return cpSlideJointSetAnchorB (constraint, *anchorB);
}

// end of cpSlideJoint.h

// start of cpPivotJoint.h

cpPivotJoint* w_cpPivotJointInit(cpPivotJoint *joint, cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB)
{
  return cpPivotJointInit (joint, a, b, *anchorA, *anchorB);
}

cpConstraint* w_cpPivotJointNew(cpBody *a, cpBody *b, cpVect *pivot)
{
  return cpPivotJointNew (a, b, *pivot);
}

cpConstraint* w_cpPivotJointNew2(cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB)
{
  return cpPivotJointNew2 (a, b, *anchorA, *anchorB);
}

void w_cpPivotJointGetAnchorA(const cpConstraint *constraint, cpVect *out)
{
  *out = cpPivotJointGetAnchorA (constraint);
}

void w_cpPivotJointSetAnchorA(cpConstraint *constraint, cpVect *anchorA)
{
  return cpPivotJointSetAnchorA (constraint, *anchorA);
}

void w_cpPivotJointGetAnchorB(const cpConstraint *constraint, cpVect *out)
{
  *out = cpPivotJointGetAnchorB (constraint);
}

void w_cpPivotJointSetAnchorB(cpConstraint *constraint, cpVect *anchorB)
{
  return cpPivotJointSetAnchorB (constraint, *anchorB);
}

// end of cpPivotJoint.h

// start of cpGrooveJoint.h

cpGrooveJoint* w_cpGrooveJointInit(cpGrooveJoint *joint, cpBody *a, cpBody *b, cpVect *groove_a, cpVect *groove_b, cpVect *anchorB)
{
  return cpGrooveJointInit (joint, a, b, *groove_a, *groove_b, *anchorB);
}

cpConstraint* w_cpGrooveJointNew(cpBody *a, cpBody *b, cpVect *groove_a, cpVect *groove_b, cpVect *anchorB)
{
  return cpGrooveJointNew (a, b, *groove_a, *groove_b, *anchorB);
}

void w_cpGrooveJointGetGrooveA(const cpConstraint *constraint, cpVect *out)
{
  *out = cpGrooveJointGetGrooveA (constraint);
}

void w_cpGrooveJointSetGrooveA(cpConstraint *constraint, cpVect *grooveA)
{
  return cpGrooveJointSetGrooveA (constraint, *grooveA);
}

void w_cpGrooveJointGetGrooveB(const cpConstraint *constraint, cpVect *out)
{
  *out = cpGrooveJointGetGrooveB (constraint);
}

void w_cpGrooveJointSetGrooveB(cpConstraint *constraint, cpVect *grooveB)
{
  return cpGrooveJointSetGrooveB (constraint, *grooveB);
}

void w_cpGrooveJointGetAnchorB(const cpConstraint *constraint, cpVect *out)
{
  *out = cpGrooveJointGetAnchorB (constraint);
}

void w_cpGrooveJointSetAnchorB(cpConstraint *constraint, cpVect *anchorB)
{
  return cpGrooveJointSetAnchorB (constraint, *anchorB);
}

// end of cpGrooveJoint.h

// start of cpDampedSpring.h

cpDampedSpring* w_cpDampedSpringInit(cpDampedSpring *joint, cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB, cpFloat restLength, cpFloat stiffness, cpFloat damping)
{
  return cpDampedSpringInit (joint, a, b, *anchorA, *anchorB, restLength, stiffness, damping);
}

cpConstraint* w_cpDampedSpringNew(cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB, cpFloat restLength, cpFloat stiffness, cpFloat damping)
{
  return cpDampedSpringNew (a, b, *anchorA, *anchorB, restLength, stiffness, damping);
}

void w_cpDampedSpringGetAnchorA(const cpConstraint *constraint, cpVect *out)
{
  *out = cpDampedSpringGetAnchorA (constraint);
}

void w_cpDampedSpringSetAnchorA(cpConstraint *constraint, cpVect *anchorA)
{
  return cpDampedSpringSetAnchorA (constraint, *anchorA);
}

void w_cpDampedSpringGetAnchorB(const cpConstraint *constraint, cpVect *out)
{
  *out = cpDampedSpringGetAnchorB (constraint);
}

void w_cpDampedSpringSetAnchorB(cpConstraint *constraint, cpVect *anchorB)
{
  return cpDampedSpringSetAnchorB (constraint, *anchorB);
}

// end of cpDampedSpring.h

// end of cpConstraint.h

// start of cpSpace.h

void w_cpSpaceGetGravity(const cpSpace *space, cpVect *out)
{
  *out = cpSpaceGetGravity (space);
}

void w_cpSpaceSetGravity(cpSpace *space, cpVect *gravity)
{
  return cpSpaceSetGravity (space, *gravity);
}

void w_cpSpacePointQuery(cpSpace *space, cpVect *point, cpFloat maxDistance, cpShapeFilter *filter, cpSpacePointQueryFunc func, void *data)
{
  return cpSpacePointQuery (space, *point, maxDistance, *filter, func, data);
}

cpShape* w_cpSpacePointQueryNearest(cpSpace *space, cpVect *point, cpFloat maxDistance, cpShapeFilter *filter, cpPointQueryInfo *out)
{
  return cpSpacePointQueryNearest (space, *point, maxDistance, *filter, out);
}

void w_cpSpaceSegmentQuery(cpSpace *space, cpVect *start, cpVect *end, cpFloat radius, cpShapeFilter *filter, cpSpaceSegmentQueryFunc func, void *data)
{
  return cpSpaceSegmentQuery (space, *start, *end, radius, *filter, func, data);
}

cpShape* w_cpSpaceSegmentQueryFirst(cpSpace *space, cpVect *start, cpVect *end, cpFloat radius, cpShapeFilter *filter, cpSegmentQueryInfo *out)
{
  return cpSpaceSegmentQueryFirst (space, *start, *end, radius, *filter, out);
}

void w_cpSpaceBBQuery(cpSpace *space, cpBB *bb, cpShapeFilter *filter, cpSpaceBBQueryFunc func, void *data)
{
  return cpSpaceBBQuery (space, *bb, *filter, func, data);
}

// end of cpSpace.h

cpFloat w_cpMomentForCircle(cpFloat m, cpFloat r1, cpFloat r2, cpVect *offset)
{
  return cpMomentForCircle (m, r1, r2, *offset);
}

cpFloat w_cpMomentForSegment(cpFloat m, cpVect *a, cpVect *b, cpFloat radius)
{
  return cpMomentForSegment (m, *a, *b, radius);
}

cpFloat w_cpAreaForSegment(cpVect *a, cpVect *b, cpFloat radius)
{
  return cpAreaForSegment (*a, *b, radius);
}

cpFloat w_cpMomentForPoly(cpFloat m, int count, const cpVect *verts, cpVect *offset, cpFloat radius)
{
  return cpMomentForPoly (m, count, verts, *offset, radius);
}

void w_cpCentroidForPoly(const int count, const cpVect *verts, cpVect *out)
{
  *out = cpCentroidForPoly (count, verts);
}

cpFloat w_cpMomentForBox2(cpFloat m, cpBB *box)
{
  return cpMomentForBox2 (m, *box);
}

void w_cpClosetPointOnSegment(const cpVect *p, const cpVect *a, const cpVect *b, cpVect *out)
{
  *out = cpClosetPointOnSegment (*p, *a, *b);
}

// start of chipmunk_unsafe.h

#include "chipmunk/chipmunk_unsafe.h"

void w_cpCircleShapeSetOffset(cpShape *shape, cpVect *offset)
{
  return cpCircleShapeSetOffset (shape, *offset);
}

void w_cpSegmentShapeSetEndpoints(cpShape *shape, cpVect *a, cpVect *b)
{
  return cpSegmentShapeSetEndpoints (shape, *a, *b);
}

void w_cpPolyShapeSetVerts(cpShape *shape, int count, cpVect *verts, cpTransform *transform)
{
  return cpPolyShapeSetVerts (shape, count, verts, *transform);
}

// end of chipmunk_unsafe.h
