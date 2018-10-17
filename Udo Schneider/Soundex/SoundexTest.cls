"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #SoundexTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SoundexTest guid: (GUID fromString: '{65cf15a9-5abd-4b3f-8261-13afaa7bfe2d}')!
SoundexTest comment: ''!
!SoundexTest categoriesForClass!Unclassified! !
!SoundexTest methodsFor!

testSoundex
	self
		assert: 'Euler' soundex = 'E460';
		assert: 'Ellery' soundex = 'E460';
		assert: 'Gauss' soundex = 'G200';
		assert: 'Gosh' soundex = 'G200';
		assert: 'Hilbert' soundex = 'H416';
		assert: 'Heilbronn' soundex = 'H416';
		assert: 'Knuth' soundex = 'K530';
		assert: 'Kant' soundex = 'K530';
		assert: 'Lloyd' soundex = 'L300';
		assert: 'Ladd' soundex = 'L300';
		assert: 'Lukasiewicz' soundex = 'L222';
		assert: 'Lissajous' soundex = 'L222'! !
!SoundexTest categoriesFor: #testSoundex!*-unreferenced selectors!public! !

