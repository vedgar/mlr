utje�eU(drava, sava).
utje�eU(sava, dunav).
utje�eU(dunav, 'crno more').

more('crno more').
uMore(Rijeka, More) :- utje�eU(Rijeka, More), more(More).
uMore(Rijeka, More) :- utje�eU(Rijeka, Druga),
		       uMore(Druga, More).