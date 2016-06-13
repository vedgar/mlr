majka(ivanka, stefanija).
majka(tamara, ivanka).
majka(vedran, ivanka).
otac(ivica, josip).
otac(vedran, ivica).
otac(ivan, vedran).

roditelj(A, B) :- otac(A, B).
roditelj(A, B) :- majka(A, B).

potomak(A, B) :- roditelj(B, A).
potomak(A, C) :- roditelj(C, B), potomak(A, B).
