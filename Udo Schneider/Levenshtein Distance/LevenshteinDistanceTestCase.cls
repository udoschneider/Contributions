"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #LevenshteinDistanceTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LevenshteinDistanceTestCase guid: (GUID fromString: '{fd206228-b1d2-4cd1-99a1-3eecf85172bc}')!
LevenshteinDistanceTestCase comment: ''!
!LevenshteinDistanceTestCase categoriesForClass!Unclassified! !
!LevenshteinDistanceTestCase methodsFor!

testLevenshteinDistance
	self
		assert: ('GUMBO' levenshteinDistance: 'GAMBOL') = 2;
		assert: ('' levenshteinDistance: '') = 0;
	assert: ('abc' levenshteinDistance: '') = 3;
	assert: ('' levenshteinDistance: 'abc') = 3;
	assert: ('abc' levenshteinDistance: 'abc') = 0;
	assert: ('McDougal' levenshteinDistance: 'MacDougall') = 2


! !
!LevenshteinDistanceTestCase categoriesFor: #testLevenshteinDistance!*-unreferenced selectors!public! !

