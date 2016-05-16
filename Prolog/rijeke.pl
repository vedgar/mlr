utjeèeU(drava, sava).
utjeèeU(sava, dunav).
utjeèeU(dunav, 'crno more').

more('crno more').
uMore(Rijeka, More) :- utjeèeU(Rijeka, More), more(More).
uMore(Rijeka, More) :- utjeèeU(Rijeka, Druga),
		       uMore(Druga, More).