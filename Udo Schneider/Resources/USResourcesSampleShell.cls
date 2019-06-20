"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #USResourcesSampleShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
USResourcesSampleShell guid: (GUID fromString: '{c55be272-f757-4c49-86df-f312fa5562fa}')!
USResourcesSampleShell comment: ''!
!USResourcesSampleShell categoriesForClass!MVP-Presenters! !
!USResourcesSampleShell methodsFor!

onViewOpened
	self fixApplicationIcons: USResourcesSessionManager applicationIcon.
	^super onViewOpened!

quit
	self exit.
	SessionManager current isRuntime ifTrue: [SessionManager current quit]! !
!USResourcesSampleShell categoriesFor: #onViewOpened!public! !
!USResourcesSampleShell categoriesFor: #quit!public! !

!USResourcesSampleShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 549 nil nil nil 416 nil 170 192 8 #() nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 2 265030 4 ##(Smalltalk.Menu) nil true 34 1 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #quit 8 'Quit' 17639 1 nil nil nil 8 'File' nil 134217729 nil nil 32599 nil nil 594 nil true 34 1 642 1 674 #showAboutDialog 8 'About' 1249 1 nil nil nil 8 'Help' nil 134217729 nil nil 32603 nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil nil 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 '!!APPLICATION' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil 850 nil true 896 8 '!!APPLICATION' 944 nil nil 1 nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 5887 21 1122 1201 801 416 1074 #updateMenuBar 528 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 11 0 0 10 0 0 0 215 13 0 0 154 1 0 0] 34 1 410 ##(Smalltalk.StaticText) 34 16 nil 416 34 2 8 1140850944 65 1248 nil nil nil 7 nil nil nil 1248 nil 8 1824742848 852486 ##(Smalltalk.NullConverter) nil nil nil 1010 138 144 34 2 1074 #createAt:extent: 34 2 1122 411 251 1122 241 39 1248 1074 #text: 34 1 8 'Hello World' 1248 1186 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 205 0 0 0 125 0 0 0 69 1 0 0 144 0 0 0] 8 #() 1122 193 193 nil 27 1568 nil 27 )! !
!USResourcesSampleShell class categoriesFor: #resource_Default_view!public!resources-views! !

