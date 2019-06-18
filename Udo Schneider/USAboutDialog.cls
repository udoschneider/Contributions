"Filed out from Dolphin Smalltalk 7"!

Dialog subclass: #USAboutDialog
	instanceVariableNames: 'imagePresenter productPresenter versionPresenter copyrightPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
USAboutDialog guid: (GUID fromString: '{c9cd4759-e5cc-4977-995c-08c29ed6c29e}')!
USAboutDialog comment: ''!
!USAboutDialog categoriesForClass!Unclassified! !
!USAboutDialog methodsFor!

additionalAccelerators
	^#(#(#exit 'ESC'))!

createComponents
	imagePresenter := self add: ImagePresenter new name: 'image'.
	productPresenter := self add: TextPresenter new name: 'product'.
	versionPresenter := self add: TextPresenter new name: 'version'.
	copyrightPresenter := self add: TextPresenter new name: 'copyright'.
	^super createComponents!

createSchematicWiring
	copyrightPresenter 
		when: #linkClicked:
		send: #onLinkClicked:
		to: self!

imageStripper
^self model owningPackage imageStripper!

model: aModel
	super model: aModel.
	imagePresenter value: (self model applicationIcon withExtent: 128 @ 128).
	productPresenter value: self model applicationName.
	versionPresenter value: self model productVersion!

onLinkClicked: aUrl 
	ShellLibrary default shellOpen: aUrl!

onViewAvailable
	(copyrightPresenter view)
		autoUrlDetect: true;
		link: true.
self updateCopyright.
	^super onViewAvailable!

onViewOpened
	self view largeIcon: self model applicationIcon.
	^super onViewOpened!

updateCaption
	self caption: self model applicationName , ' ' , self model productVersion!

updateCopyright
	| rtfStream |
	rtfStream := ReadWriteStream on: String new.
	rtfStream
		nextPutAll: '{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fswiss\fcharset0 Arial;}}
\viewkind4\uc1\pard\lang1031\f0';
		nextPutAll: '\fs16 ';
		nextPutAll: self model fullDescription;
		nextPutAll: '\par';
		nextPutAll: '\par \li0 \b Author:\b0 ';
		nextPutAll: '\par \li710 ';
		nextPutAll: self model author.
	self model email ifNotNil: [:value | rtfStream nextPutAll: ' <mailto:' , value , '>'].
	rtfStream nextPutAll: '\par'.
	self model homepage 
		ifNotNil: 
			[:value | 
			rtfStream
				nextPutAll: '\par \li0 \b Homepage: \b0 ';
				nextPutAll: '\par \li710 ';
				nextPutAll: value;
				nextPutAll: '\par'].
	rtfStream nextPutAll: '\par \li0 \b Copyright:\b0 '.
	rtfStream nextPutAll: '\par \li710 '.
	self model fullCopyright lines do: 
			[:each | 
			rtfStream
				nextPutAll: each;
				nextPutAll: '\par '].
	rtfStream nextPutAll: '\lang1033 \par }'.
	copyrightPresenter value: (RichText fromRtf: rtfStream contents).
	copyrightPresenter view enableMailtoLinks! !
!USAboutDialog categoriesFor: #additionalAccelerators!public! !
!USAboutDialog categoriesFor: #createComponents!*-in class package!public! !
!USAboutDialog categoriesFor: #createSchematicWiring!*-in class package!public! !
!USAboutDialog categoriesFor: #imageStripper!*-in class package!public! !
!USAboutDialog categoriesFor: #model:!*-in class package!public! !
!USAboutDialog categoriesFor: #onLinkClicked:!*-in class package!public! !
!USAboutDialog categoriesFor: #onViewAvailable!*-in class package!public! !
!USAboutDialog categoriesFor: #onViewOpened!*-in class package!public! !
!USAboutDialog categoriesFor: #updateCaption!*-in class package!public! !
!USAboutDialog categoriesFor: #updateCopyright!*-in class package!public! !

!USAboutDialog class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.DialogView)  98 30 0 0 98 2 26214401 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 167 0 0 0 416 852230 ##(Smalltalk.FramingLayout)  234 240 98 8 410 8 ##(Smalltalk.StaticIcon)  98 17 0 416 98 2 8 1140850947 1 592 721990 2 ##(Smalltalk.ValueHolder)  0 0 1376774 ##(Smalltalk.PluggableSearchPolicy)  459270 ##(Smalltalk.Message)  8 #== 98 0 738 8 #hash 98 0 0 0 0 7 0 0 0 592 0 8 4294906275 852486 ##(Smalltalk.NullConverter)  0 0 32 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  331 11 1026 97 97 592 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 165 0 0 0 5 0 0 0 213 0 0 0 53 0 0 0] 98 0 1026 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints)  1180678 ##(Smalltalk.FramingCalculation)  8 #fixedParentLeft 331 1186 8 #fixedParentRight -329 1186 8 #fixedParentTop 11 1186 8 #fixedViewTop 97 410 8 ##(Smalltalk.StaticText)  98 16 0 416 98 2 8 1140850945 1 1328 0 0 0 7 0 0 0 1328 0 8 4294906275 866 0 0 0 898 202 208 98 2 962 992 98 2 1026 1 167 1026 757 31 1328 962 8 #text: 98 1 8 'Version
' 1328 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 83 0 0 0 122 1 0 0 98 0 0 0] 98 0 1136 0 27 1154 1200 1 1232 1 1186 8 #fixedPreviousBottom 1 1296 31 410 8 ##(Smalltalk.RichTextEdit)  98 18 0 416 98 2 8 1142947908 262145 1712 674 0 32 1310726 ##(Smalltalk.EqualitySearchPolicy)  0 482 8 4278190080 0 1031 0 263174 ##(Smalltalk.Font)  0 16 459014 ##(Smalltalk.LOGFONT)  8 #[247 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1026 193 193 0 1712 0 8 1958431269 866 0 0 3 0 655622 ##(Smalltalk.EDITSTREAM)  8 #[0 0 0 0 0 0 0 0 64 0 163 2] 898 202 208 98 5 962 992 98 2 1026 1 207 1026 757 511 1712 962 1568 98 1 524550 ##(Smalltalk.RichText)  8 '{\rtf1\ansi\ansicpg1252\deff0{\fonttbl{\f0\fswiss\fprq2\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\lang1031\f0\fs14 Copyright
\par }
' 1712 962 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 1712 962 8 #isTextModified: 98 1 16 1712 962 8 #resetCharFormat 98 0 1712 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 103 0 0 0 122 1 0 0 102 1 0 0] 98 0 1136 0 27 1154 1200 1 1232 1 1680 11 1186 8 #fixedParentBottom 1 410 1344 98 16 0 416 98 2 8 1140850945 1 2512 0 0 0 7 0 1874 0 16 1906 8 #[235 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 1952 0 2512 0 8 4294906275 866 0 0 0 898 202 208 98 2 962 992 98 2 1026 1 117 1026 757 51 2512 962 1568 98 1 8 'Product' 2512 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 58 0 0 0 122 1 0 0 83 0 0 0] 98 0 1136 0 27 1154 1200 1 1232 1 1680 11 1296 51 234 256 98 8 592 8 'image' 1328 8 'version' 1712 8 'copyright' 2512 8 'product' 0 0 0 0 0 3 0 0 0 0 1 0 0 590598 ##(Smalltalk.Semaphore)  0 0 1 0 8 1991583416 898 202 208 98 2 962 992 98 2 1026 3359 21 1026 769 769 416 962 8 #updateMenuBar 2400 416 1074 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 143 6 0 0 10 0 0 0 15 8 0 0 138 1 0 0] 98 4 592 2512 1328 1712 1136 0 27 )! !
!USAboutDialog class categoriesFor: #resource_Default_view!*-in class package!public!resources-views! !

