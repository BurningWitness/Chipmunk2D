#include "chipmunk/chipmunk.h"

// start of cpVect.h

void w_cpvzero(cpVect *out);

void w_cpv(const cpFloat x, const cpFloat y, cpVect *out);

cpBool w_cpveql(const cpVect *v1, const cpVect *v2);

void w_cpvadd(const cpVect *v1, const cpVect *v2, cpVect *out);

void w_cpvsub(const cpVect *v1, const cpVect *v2, cpVect *out);

void w_cpvneg(const cpVect *v, cpVect *out);

void w_cpvmult(const cpVect *v, const cpFloat s, cpVect *out);

cpFloat w_cpvdot(const cpVect *v1, const cpVect *v2);

cpFloat w_cpvcross(const cpVect *v1, const cpVect *v2);

void w_cpvperp(const cpVect *v, cpVect *out);

void w_cpvrperp(const cpVect *v, cpVect *out);

void w_cpvproject(const cpVect *v1, const cpVect *v2, cpVect *out);

void w_cpvforangle(const cpFloat a, cpVect *out);

cpFloat w_cpvtoangle(const cpVect *v);

void w_cpvrotate(const cpVect *v1, const cpVect *v2, cpVect *out);

void w_cpvunrotate(const cpVect *v1, const cpVect *v2, cpVect *out);

cpFloat w_cpvlengthsq(const cpVect *v);

cpFloat w_cpvlength(const cpVect *v);

void w_cpvlerp(const cpVect *v1, const cpVect *v2, const cpFloat t, cpVect *out);

void w_cpvnormalize(const cpVect *v, cpVect *out);

void w_cpvslerp(const cpVect *v1, const cpVect *v2, const cpFloat t, cpVect *out);

void w_cpvslerpconst(const cpVect *v1, const cpVect *v2, const cpFloat a, cpVect *out);

void w_cpvclamp(const cpVect *v, const cpFloat len, cpVect *out);

void w_cpvlerpconst(cpVect *v1, cpVect *v2, cpFloat d, cpVect *out);

cpFloat w_cpvdist(const cpVect *v1, const cpVect *v2);

cpFloat w_cpvdistsq(const cpVect *v1, const cpVect *v2);

cpBool w_cpvnear(const cpVect *v1, const cpVect *v2, const cpFloat dist);



void w_cpMat2x2New(cpFloat a, cpFloat b, cpFloat c, cpFloat d, cpMat2x2 *out);

void w_cpMat2x2Transform(cpMat2x2 *m, cpVect *v, cpVect *out);

// end of cpVect.h

// start of cpBB.h

void w_cpBBNew(const cpFloat l, const cpFloat b, const cpFloat r, const cpFloat t, cpBB *out);

void w_cpBBNewForExtents(const cpVect *c, const cpFloat hw, const cpFloat hh, cpBB *out);

void w_cpBBNewForCircle(const cpVect *p, const cpFloat r, cpBB *out);

cpBool w_cpBBIntersects(const cpBB *a, const cpBB *b);

cpBool w_cpBBContainsBB(const cpBB *bb, const cpBB *other);

cpBool w_cpBBContainsVect(const cpBB *bb, const cpVect *v);

void w_cpBBMerge(const cpBB *a, const cpBB *b, cpBB *out);

void w_cpBBExpand(const cpBB *bb, const cpVect *v, cpBB *out);

void w_cpBBCenter(cpBB *bb, cpVect *out);

cpFloat w_cpBBArea(cpBB *bb);

cpFloat w_cpBBMergedArea(cpBB *a, cpBB *b);

cpFloat w_cpBBSegmentQuery(cpBB *bb, cpVect *a, cpVect *b);

cpBool w_cpBBIntersectsSegment(cpBB *bb, cpVect *a, cpVect *b);

void w_cpBBClampVect(const cpBB *bb, const cpVect *v, cpVect *out);

void w_cpBBWrapVect(const cpBB *bb, const cpVect *v, cpVect *out);

void w_cpBBOffset(const cpBB *bb, const cpVect *v, cpBB *out);

// end of cpBB.h

// start of cpTransform.h

void w_cpTransformIdentity (cpTransform *out);

void w_cpTransformNew(cpFloat a, cpFloat b, cpFloat c, cpFloat d, cpFloat tx, cpFloat ty, cpTransform *out);

void w_cpTransformNewTranspose(cpFloat a, cpFloat c, cpFloat tx, cpFloat b, cpFloat d, cpFloat ty, cpTransform *out);

void w_cpTransformInverse(cpTransform *t, cpTransform *out);

void w_cpTransformMult(cpTransform *t1, cpTransform *t2, cpTransform *out);

void w_cpTransformPoint(cpTransform *t, cpVect *p, cpVect *out);

void w_cpTransformVect(cpTransform *t, cpVect *v, cpVect *out);

void w_cpTransformbBB(cpTransform *t, cpBB *bb, cpBB *out);

void w_cpTransformTranslate(cpVect *translate, cpTransform *out);

void w_cpTransformScale(cpFloat scaleX, cpFloat scaleY, cpTransform *out);

void w_cpTransformRotate(cpFloat radians, cpTransform *out);

void w_cpTransformRigid(cpVect *translate, cpFloat radians, cpTransform *out);

void w_cpTransformRigidInverse(cpTransform *t, cpTransform *out);

void w_cpTransformWrap(cpTransform *outer, cpTransform *inner, cpTransform *out);

void w_cpTransformWrapInverse(cpTransform *outer, cpTransform *inner, cpTransform *out);

void w_cpTransformOrtho(cpBB *bb, cpTransform *out);

void w_cpTransformBoneScale(cpVect *v0, cpVect *v1, cpTransform *out);

void w_cpTransformAxialScale(cpVect *axis, cpVect *pivot, cpFloat scale, cpTransform *out);

// end of cpTransform.h

// start of cpSpatialindex.h

void w_cpSpatialIndexQuery(cpSpatialIndex *index, void *obj, cpBB *bb, cpSpatialIndexQueryFunc func, void *data);

void w_cpSpatialIndexSegmentQuery(cpSpatialIndex *index, void *obj, cpVect *a, cpVect *b, cpFloat t_exit, cpSpatialIndexSegmentQueryFunc func, void *data);

// end of cpSpatialIndex.h

// start of cpArbiter.h

void w_cpArbiterGetContactPointSet(const cpArbiter *arb, cpContactPointSet *out);

void w_cpArbiterGetSurfaceVelocity(cpArbiter *arb, cpVect *out);

void w_cpArbiterSetSurfaceVelocity(cpArbiter *arb, cpVect *vr);

void w_cpArbiterTotalImpulse(const cpArbiter *arb, cpVect *out);

void w_cpArbiterGetNormal(const cpArbiter *arb, cpVect *out);

void w_cpArbiterGetPointA(const cpArbiter *arb, int i, cpVect *out);

void w_cpArbiterGetPointB(const cpArbiter *arb, int i, cpVect *out);

// end of cpArbiter.h

// start of cpBody.h

void w_cpBodyGetPosition(const cpBody *body, cpVect *out);

void w_cpBodySetPosition(cpBody *body, cpVect *pos);

void w_cpBodyGetCenterOfGravity(const cpBody *body, cpVect *out);

void w_cpBodySetCenterOfGravity(cpBody *body, cpVect* cog);

void w_cpBodyGetVelocity(const cpBody *body, cpVect *out);

void w_cpBodySetVelocity(cpBody *body, cpVect *velocity);

void w_cpBodyGetForce(const cpBody *body, cpVect *out);

void w_cpBodySetForce(cpBody *body, cpVect *force);

void w_cpBodyGetRotation(const cpBody *body, cpVect *out);

void w_cpBodyUpdateVelocity(cpBody *body, cpVect *gravity, cpFloat damping, cpFloat dt);

void w_cpBodyLocalToWorld(const cpBody *body, const cpVect *point, cpVect *out);

void w_cpBodyWorldToLocal(const cpBody *body, const cpVect *point, cpVect *out);

void w_cpBodyApplyForceAtWorldPoint(cpBody *body, cpVect *force, cpVect *point);

void w_cpBodyApplyForceAtLocalPoint(cpBody *body, cpVect *force, cpVect *point);

void w_cpBodyApplyImpulseAtWorldPoint(cpBody *body, cpVect *impulse, cpVect *point);

void w_cpBodyApplyImpulseAtLocalPoint(cpBody *body, cpVect *impulse, cpVect *point);

void w_cpBodyGetVelocityAtWorldPoint(const cpBody *body, cpVect *point, cpVect *out);

void w_cpBodyGetVelocityAtLocalPoint(const cpBody *body, cpVect *point, cpVect *out);

// end of cpBody.h

// start of cpShape.h

void w_cp_shape_filter_all (cpShapeFilter *out);

void w_cp_shape_filter_none (cpShapeFilter *out);

void w_cpShapeCacheBB(cpShape *shape, cpBB *out);

void w_cpShapeUpdate(cpShape *shape, cpTransform *transform, cpBB *out);

cpFloat w_cpShapePointQuery(const cpShape *shape, cpVect *p, cpPointQueryInfo *out);

cpBool w_cpShapeSegmentQuery(const cpShape *shape, cpVect *a, cpVect *b, cpFloat radius, cpSegmentQueryInfo *info);

void w_cpShapesCollide(const cpShape *a, const cpShape *b, cpContactPointSet *out);

void w_cpShapeGetCenterOfGravity(cpShape *shape, cpVect *out);

void w_cpShapeGetBB(const cpShape *shape, cpBB *out);

void w_cpShapeGetSurfaceVelocity(const cpShape *shape, cpVect *out);

void w_cpShapeSetSurfaceVelocity(cpShape *shape, cpVect *surfaceVelocity);

void w_cpShapeGetFilter(const cpShape *shape, cpShapeFilter *out);

void w_cpShapeSetFilter(cpShape *shape, cpShapeFilter *filter);

cpCircleShape* w_cpCircleShapeInit(cpCircleShape *circle, cpBody *body, cpFloat radius, cpVect *offset);

cpShape* w_cpCircleShapeNew(cpBody *body, cpFloat radius, cpVect *offset);

void w_cpCircleShapeGetOffset(const cpShape *shape, cpVect *out);

cpSegmentShape* w_cpSegmentShapeInit(cpSegmentShape *seg, cpBody *body, cpVect *a, cpVect *b, cpFloat radius);

cpShape* w_cpSegmentShapeNew(cpBody *body, cpVect *a, cpVect *b, cpFloat radius);

void w_cpSegmentShapeSetNeighbors(cpShape *shape, cpVect *prev, cpVect *next);

void w_cpSegmentShapeGetA(const cpShape *shape, cpVect *out);

void w_cpSegmentShapeGetB(const cpShape *shape, cpVect *out);

void w_cpSegmentShapeGetNormal(const cpShape *shape, cpVect *out);

// end of cpShape.h

// start of cpPolyShape.h

cpPolyShape* w_cpPolyShapeInit(cpPolyShape *poly, cpBody *body, int count, const cpVect *verts, cpTransform *transform, cpFloat radius);

cpShape* w_cpPolyShapeNew(cpBody *body, int count, const cpVect *verts, cpTransform *transform, cpFloat radius);

cpPolyShape* w_cpBoxShapeInit2(cpPolyShape *poly, cpBody *body, cpBB *box, cpFloat radius);

cpShape* w_cpBoxShapeNew2(cpBody *body, cpBB *box, cpFloat radius);

void w_cpPolyShapeGetVert(const cpShape *shape, int index, cpVect *out);

// end of cpPolyShape.h

// start of cpConstraint.h

// start of cpPinJoint.h

cpPinJoint* w_cpPinJointInit(cpPinJoint *joint, cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB);

cpConstraint* w_cpPinJointNew(cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB);

void w_cpPinJointGetAnchorA(const cpConstraint *constraint, cpVect *out);

void w_cpPinJointSetAnchorA(cpConstraint *constraint, cpVect *anchorA);

void w_cpPinJointGetAnchorB(const cpConstraint *constraint, cpVect *out);

void w_cpPinJointSetAnchorB(cpConstraint *constraint, cpVect *anchorB);

// end of cpPinJoint.h

// start of cpSlideJoint.h

cpSlideJoint* w_cpSlideJointInit(cpSlideJoint *joint, cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB, cpFloat min, cpFloat max);

cpConstraint* w_cpSlideJointNew(cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB, cpFloat min, cpFloat max);

void w_cpSlideJointGetAnchorA(const cpConstraint *constraint, cpVect *out);

void w_cpSlideJointSetAnchorA(cpConstraint *constraint, cpVect *anchorA);

void w_cpSlideJointGetAnchorB(const cpConstraint *constraint, cpVect *out);

void w_cpSlideJointSetAnchorB(cpConstraint *constraint, cpVect *anchorB);

// end of cpSlideJoint.h

// start of cpPivotJoint.h

cpPivotJoint* w_cpPivotJointInit(cpPivotJoint *joint, cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB);

cpConstraint* w_cpPivotJointNew(cpBody *a, cpBody *b, cpVect *pivot);

cpConstraint* w_cpPivotJointNew2(cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB);

void w_cpPivotJointGetAnchorA(const cpConstraint *constraint, cpVect *out);

void w_cpPivotJointSetAnchorA(cpConstraint *constraint, cpVect *anchorA);

void w_cpPivotJointGetAnchorB(const cpConstraint *constraint, cpVect *out);

void w_cpPivotJointSetAnchorB(cpConstraint *constraint, cpVect *anchorB);

// end of cpPivotJoint.h

// start of cpGrooveJoint.h

cpGrooveJoint* w_cpGrooveJointInit(cpGrooveJoint *joint, cpBody *a, cpBody *b, cpVect *groove_a, cpVect *groove_b, cpVect *anchorB);

cpConstraint* w_cpGrooveJointNew(cpBody *a, cpBody *b, cpVect *groove_a, cpVect *groove_b, cpVect *anchorB);

void w_cpGrooveJointGetGrooveA(const cpConstraint *constraint, cpVect *out);

void w_cpGrooveJointSetGrooveA(cpConstraint *constraint, cpVect *grooveA);

void w_cpGrooveJointGetGrooveB(const cpConstraint *constraint, cpVect *out);

void w_cpGrooveJointSetGrooveB(cpConstraint *constraint, cpVect *grooveB);

void w_cpGrooveJointGetAnchorB(const cpConstraint *constraint, cpVect *out);

void w_cpGrooveJointSetAnchorB(cpConstraint *constraint, cpVect *anchorB);

// end of cpGrooveJoint.h

// start of cpDampedSpring.h

cpDampedSpring* w_cpDampedSpringInit(cpDampedSpring *joint, cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB, cpFloat restLength, cpFloat stiffness, cpFloat damping);

cpConstraint* w_cpDampedSpringNew(cpBody *a, cpBody *b, cpVect *anchorA, cpVect *anchorB, cpFloat restLength, cpFloat stiffness, cpFloat damping);

void w_cpDampedSpringGetAnchorA(const cpConstraint *constraint, cpVect *out);

void w_cpDampedSpringSetAnchorA(cpConstraint *constraint, cpVect *anrhoA);

void w_cpDampedSpringGetAnchorB(const cpConstraint *constraint, cpVect *out);

void w_cpDampedSpringSetAnchorB(cpConstraint *constraint, cpVect *anchorB);

// end of cpDampedSpring.h

// end of cpConstraint.h

// start of cpSpace.h

void w_cpSpaceGetGravity(const cpSpace *space, cpVect *out);

void w_cpSpaceSetGravity(cpSpace *space, cpVect *gravity);

void w_cpSpacePointQuery(cpSpace *space, cpVect *point, cpFloat maxDistance, cpShapeFilter *filter, cpSpacePointQueryFunc func, void *data);

cpShape* w_cpSpacePointQueryNearest(cpSpace *space, cpVect *point, cpFloat maxDistance, cpShapeFilter *filter, cpPointQueryInfo *out);

void w_cpSpaceSegmentQuery(cpSpace *space, cpVect *start, cpVect *end, cpFloat radius, cpShapeFilter *filter, cpSpaceSegmentQueryFunc func, void *data);

cpShape* w_cpSpaceSegmentQueryFirst(cpSpace *space, cpVect *start, cpVect *end, cpFloat radius, cpShapeFilter *filter, cpSegmentQueryInfo *out);

void w_cpSpaceBBQuery(cpSpace *space, cpBB *bb, cpShapeFilter *filter, cpSpaceBBQueryFunc func, void *data);

// end of cpSpace.h

cpFloat w_cpMomentForCircle(cpFloat m, cpFloat r1, cpFloat r2, cpVect *offset);

cpFloat w_cpMomentForSegment(cpFloat m, cpVect *a, cpVect *b, cpFloat radius);

cpFloat w_cpAreaForSegment(cpVect *a, cpVect *b, cpFloat radius);

cpFloat w_cpMomentForPoly(cpFloat m, int count, const cpVect *verts, cpVect *offset, cpFloat radius);

void w_cpCentroidForPoly(const int count, const cpVect *verts, cpVect *out);

cpFloat w_cpMomentForBox2(cpFloat m, cpBB *box);

void w_cpClosetPointOnSegment(const cpVect *p, const cpVect *a, const cpVect *b, cpVect *out);

// start of chipmunk_unsafe.h

#include "chipmunk/chipmunk_unsafe.h"

void w_cpCircleShapeSetOffset(cpShape *shape, cpVect *offset);

void w_cpSegmentShapeSetEndpoints(cpShape *shape, cpVect *a, cpVect *b);

void w_cpPolyShapeSetVerts(cpShape *shape, int count, cpVect *verts, cpTransform *transform);

// end of chipmunk_unsafe.h
