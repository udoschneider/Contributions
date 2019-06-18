"Filed out from Dolphin Smalltalk 7"!

Object subclass: #CDDemo
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CDDemo guid: (GUID fromString: '{44bdd8e4-9bc6-41aa-b7f3-22822483fe98}')!
CDDemo comment: 'This class demonstrates use of the WAX library.'!
!CDDemo categoriesForClass!Unclassified! !
!CDDemo class methodsFor!

main	"demonstrates using the WAX API"	"Run this with CDDemo main"	| wax |	wax := WAX new.	wax start: 'foo';		 text: 'bar';		 end;		 close! !
!CDDemo class categoriesFor: #main!public! !

