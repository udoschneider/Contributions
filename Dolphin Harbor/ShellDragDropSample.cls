"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #ShellDragDropSample
	instanceVariableNames: 'textPresenter fullTextPresenter listPresenter listBoxPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ShellDragDropSample guid: (GUID fromString: '{ac1db0e4-af7f-4950-bd45-66bd0436dcc3}')!
ShellDragDropSample comment: 'ShellDragDropSample is a sample shell to demonstrates the use of Shell data transfer using COM drag and drop.

	ShellDragDropSample  show.'!
!ShellDragDropSample categoriesForClass!Unclassified! !
!ShellDragDropSample methodsFor!

clearAll
	textPresenter model: '' asValue.
	fullTextPresenter model: '' asValue.
	listPresenter clear.
	listBoxPresenter clear!

contentsOf: aFilename
	"self halt."

	| fs contents |
	^
	[fs := FileStream read: aFilename text: true.
	[contents := fs contents] ensure: [fs close].
	contents] 
			on: Error
			do: [:e | '']!

createComponents
	super createComponents.
	textPresenter := self add: TextPresenter new name: 'text'.
	fullTextPresenter := self add: TextPresenter new name: 'fullText'.
	listPresenter := self add: ListPresenter new name: 'list'.
	listBoxPresenter := self add: ListPresenter new name: 'listBox'!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	self when: #closeRequested: send: #onCloseRequested: to: self.

	listPresenter
		when: #dragOver:
			send: #onDragOverList:
			to: self;
		when: #drop:
			send: #onDropOverList:
			to: self.
	listBoxPresenter
		when: #dragOver:
			send: #onDragOverListBox:
			to: self;
		when: #drop:
			send: #onDropOverListBox:
			to: self.
	fullTextPresenter
		when: #dragOver:
			send: #onDragOverFullText:
			to: self;
		when: #drop:
			send: #onDropOverFullText:
			to: self!

helpAbout
	(MessageBox new)
		caption: 'About ' , self class name asString;
		icon: self class icon;
		text: self class aboutText;
		open!

onCloseRequested
	ShellDragDropSession revokeDropTarget: self view.
	^super onCloseRequested!

onCloseRequested: aValue
	ShellDragDropSession revokeDropTarget: self view.
	aValue value: true!

onDragOverFullText: aDragDropSession

	aDragDropSession 
		operation: ((aDragDropSession dragObjects 
				allSatisfy: [:each | (each isFormatAvailable: #Filenames) and: [(each format: #Filenames) size = 1]]) 
					ifTrue: [aDragDropSession intendedOperation]
					ifFalse: [])!

onDragOverList: aDragDropSession
	aDragDropSession operation: ((aDragDropSession isFormatAvailable: #Filenames) 
				ifTrue: [aDragDropSession intendedOperation]
				ifFalse: [])!

onDragOverListBox: aDragDropSession
	aDragDropSession 
		operation: ((aDragDropSession dragObjects 
				allSatisfy: [:each | (each isFormatAvailable: #Filenames) and: [(each format: #Filenames) size = 1]]) 
					ifTrue: [aDragDropSession intendedOperation]
					ifFalse: [])!

onDropOverFullText: aSession
	| object |
	aSession dragObjects do: 
			[:each | 
			(each isFormatAvailable: #Filenames) 
				ifTrue: 
					[object := each format: #Filenames.
					each format: #String
						data: (object isEmpty ifTrue: [''] ifFalse: [self contentsOf: object first])]]!

onDropOverList: aSession
	aSession dragObjects do: 
			[:each | 
			(each isFormatAvailable: #Filenames) 
				ifTrue: 
					[| object |
					object := each format: #Filenames.
					aSession suggestedTarget isNil 
						ifTrue: [listPresenter model addAll: object]
						ifFalse: [listPresenter model addAll: object after: aSession suggestedTarget]]]!

onDropOverListBox: aSession
	aSession dragObjects do: 
			[:each | 
			(each isFormatAvailable: #Filenames) 
				ifTrue: 
					[| object |
					object := each format: #Filenames.
					aSession suggestedTarget isNil 
						ifTrue: [listBoxPresenter model addAll: object]
						ifFalse: [listBoxPresenter model addAll: object after: aSession suggestedTarget]]]!

onViewOpened
	super onViewOpened.
	ShellDragDropSession registerDropTarget: self view! !
!ShellDragDropSample categoriesFor: #clearAll!commands!public! !
!ShellDragDropSample categoriesFor: #contentsOf:!event handling!public! !
!ShellDragDropSample categoriesFor: #createComponents!initializing!public! !
!ShellDragDropSample categoriesFor: #createSchematicWiring!initializing!public! !
!ShellDragDropSample categoriesFor: #helpAbout!commands!public! !
!ShellDragDropSample categoriesFor: #onCloseRequested!event handling!public! !
!ShellDragDropSample categoriesFor: #onCloseRequested:!event handling!public! !
!ShellDragDropSample categoriesFor: #onDragOverFullText:!event handling!public! !
!ShellDragDropSample categoriesFor: #onDragOverList:!event handling!public! !
!ShellDragDropSample categoriesFor: #onDragOverListBox:!event handling!public! !
!ShellDragDropSample categoriesFor: #onDropOverFullText:!event handling!public! !
!ShellDragDropSample categoriesFor: #onDropOverList:!event handling!public! !
!ShellDragDropSample categoriesFor: #onDropOverListBox:!event handling!public! !
!ShellDragDropSample categoriesFor: #onViewOpened!event handling!public! !

!ShellDragDropSample class methodsFor!

aboutText
	^'ShellDragDropSample 

Copyright: 2002 
  Louis Sumberg <lsumberg@mindspring.com>
  Steve Waring <swaring@ozemail.com.au>

Version: %1 

DISCLAIMER: This software is freely provided purely as a sample and as such it
is provided "as is", WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE' 
		formatWith: SessionManager current versionInfo productVersionString!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 517 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 16 234 256 576 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 3 265030 4 ##(Smalltalk.Menu)  0 16 98 1 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #exit 8 'E&xit' 1 1 0 0 0 8 '&File' 0 1 0 0 23679 0 0 658 0 16 98 1 706 1 738 8 #clearAll 8 '&Clear All' 1 1 0 0 0 8 '&Edit' 0 1 0 0 23683 0 0 658 0 16 98 1 706 1 738 8 #helpAbout 8 'About' 1 1 0 0 0 8 'Help' 0 1 0 0 23687 0 0 8 '' 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  89 117 1186 2101 1449 416 1122 8 #text: 98 1 8 'Shell DragDrop Sample' 416 1122 8 #updateMenuBar 576 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 44 0 0 0 58 0 0 0 70 4 0 0 14 3 0 0] 98 3 410 8 ##(Smalltalk.ContainerView)  98 15 0 416 98 2 8 1140850688 131073 1392 0 482 512 0 5 0 0 0 1392 530 234 240 576 32 234 256 576 0 1058 202 208 98 1 1122 1152 98 2 1186 1 1 1186 1173 199 1392 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 74 2 0 0 99 0 0 0] 98 3 410 1408 98 15 0 1392 98 2 8 1140850688 131073 1696 0 482 512 0 5 0 0 0 1696 788230 ##(Smalltalk.BorderLayout)  1 1 410 8 ##(Smalltalk.StaticText)  98 16 0 1696 98 2 8 1140850945 1 1808 0 0 0 5 0 0 0 1808 0 8 4294902319 852486 ##(Smalltalk.NullConverter)  0 0 0 1058 202 208 98 2 1122 1152 98 2 1186 1 1 1186 583 51 1808 1122 1248 98 1 8 'First Filename' 1808 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 25 0 0 0] 98 0 1186 193 193 0 27 0 0 0 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 1696 98 2 8 1143017796 1025 2160 0 482 512 0 21 0 0 0 2160 0 8 4294902701 1906 0 0 9 1058 202 208 98 3 1122 1152 98 2 1186 1 51 1186 583 149 2160 1122 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 2160 1122 8 #isTextModified: 98 1 32 2160 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 35 1 0 0 99 0 0 0] 98 0 2144 0 27 234 256 98 2 2160 8 'text' 0 1058 202 208 98 1 1122 1152 98 2 1186 1 1 1186 583 199 1696 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 99 0 0 0] 98 2 1808 2160 2144 0 27 410 8 ##(Smalltalk.Splitter)  98 12 0 1392 98 2 8 1140850688 1 2784 0 482 512 0 517 0 0 0 2784 1058 202 208 98 1 1122 1152 98 2 1186 583 1 1186 7 199 2784 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 1 0 0 0 0 0 0 38 1 0 0 99 0 0 0] 98 0 2144 0 27 410 1408 98 15 0 1392 98 2 8 1140850688 131073 3040 0 482 512 0 5 0 0 0 3040 1778 1 1 410 1824 98 16 0 3040 98 2 8 1140850945 1 3136 0 0 0 5 0 0 0 3136 0 8 4294902319 1906 0 0 0 1058 202 208 98 2 1122 1152 98 2 1186 1 1 1186 585 51 3136 1122 1248 98 1 8 'First Contents (one file only)' 3136 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 36 1 0 0 25 0 0 0] 98 0 2144 0 27 0 0 0 410 2176 98 16 0 3040 98 2 8 1143017796 1025 3440 0 482 512 0 21 0 0 0 3440 0 8 4294902701 1906 0 0 9 1058 202 208 98 3 1122 1152 98 2 1186 1 51 1186 585 149 3440 1122 2416 98 1 2450 3 1 3 3440 1122 2496 98 1 32 3440 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 36 1 0 0 99 0 0 0] 98 0 2144 0 27 234 256 98 2 3440 8 'fullText' 0 1058 202 208 98 1 1122 1152 98 2 1186 589 1 1186 585 199 3040 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 1 0 0 0 0 0 0 74 2 0 0 99 0 0 0] 98 2 3136 3440 2144 0 27 2144 0 27 410 2800 98 12 0 416 98 2 8 1140850688 1 4000 0 482 512 0 517 0 0 0 4000 1058 202 208 98 1 1122 1152 98 2 1186 1 199 1186 1173 7 4000 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 99 0 0 0 74 2 0 0 102 0 0 0] 98 0 2144 0 27 410 1408 98 15 0 416 98 2 8 1140850688 131073 4240 0 482 512 0 5 0 0 0 4240 530 234 240 576 32 234 256 576 0 1058 202 208 98 1 1122 1152 98 2 1186 1 205 1186 1173 201 4240 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 102 0 0 0 74 2 0 0 202 0 0 0] 98 3 410 1408 98 15 0 4240 98 2 8 1140850688 131073 4528 0 482 512 0 5 0 0 0 4528 1778 1 1 410 1824 98 16 0 4528 98 2 8 1140850945 1 4624 0 0 0 5 0 0 0 4624 0 8 4294902319 1906 0 0 0 1058 202 208 98 2 1122 1152 98 2 1186 1 1 1186 583 51 4624 1122 1248 98 1 8 'Add Filenames' 4624 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 25 0 0 0] 98 0 2144 0 27 0 0 0 410 8 ##(Smalltalk.ListView)  98 30 0 4528 98 2 8 1140920397 1025 4928 590662 2 ##(Smalltalk.ListModel)  202 208 576 0 1114638 ##(Smalltalk.STBSingletonProxy)  8 ##(Smalltalk.SearchPolicy)  8 #identity 482 512 0 21 0 0 0 4928 0 8 4294902199 8 ##(Smalltalk.BasicListAbstract)  8 ##(Smalltalk.IconicListAbstract)  5066 8 ##(Smalltalk.IconImageManager)  8 #current 0 0 0 0 0 0 202 208 98 1 920646 5 ##(Smalltalk.ListViewColumn)  8 'Column 1' 575 8 #left 5152 8 ##(Smalltalk.SortedCollection)  0 0 4928 0 3 0 0 8 #report 576 0 131169 0 0 1058 202 208 98 2 1122 1152 98 2 1186 1 51 1186 583 151 4928 1122 1248 98 1 8 'Column 1' 4928 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 35 1 0 0 100 0 0 0] 98 0 2144 0 27 234 256 98 2 4928 8 'list' 0 1058 202 208 98 1 1122 1152 98 2 1186 1 1 1186 583 201 4528 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 35 1 0 0 100 0 0 0] 98 2 4624 4928 2144 0 27 410 2800 98 12 0 4240 98 2 8 1140850688 1 5776 0 482 512 0 517 0 0 0 5776 1058 202 208 98 1 1122 1152 98 2 1186 583 1 1186 7 201 5776 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 35 1 0 0 0 0 0 0 38 1 0 0 100 0 0 0] 98 0 2144 0 27 410 1408 98 15 0 4240 98 2 8 1140850688 131073 6016 0 482 512 0 5 0 0 0 6016 1778 1 1 410 1824 98 16 0 6016 98 2 8 1140850945 1 6112 0 0 0 5 0 0 0 6112 0 8 4294902319 1906 0 0 0 1058 202 208 98 2 1122 1152 98 2 1186 1 1 1186 585 51 6112 1122 1248 98 1 8 'Add Filename (one file only)' 6112 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 36 1 0 0 25 0 0 0] 98 0 2144 0 27 0 0 0 410 8 ##(Smalltalk.ListBox)  98 17 0 6016 98 2 8 1144062209 1025 6416 5010 202 208 576 0 5066 5088 5104 482 512 0 21 0 0 0 6416 0 8 4294902777 5152 576 32 1058 202 208 98 2 1122 1152 98 2 1186 1 51 1186 585 151 6416 1122 8 #horizontalExtent: 98 1 1 6416 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 36 1 0 0 100 0 0 0] 98 0 2144 0 27 234 256 98 2 6416 8 'listBox' 0 1058 202 208 98 1 1122 1152 98 2 1186 589 1 1186 585 201 6016 1330 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 38 1 0 0 0 0 0 0 74 2 0 0 100 0 0 0] 98 2 6112 6416 2144 0 27 2144 0 27 2144 0 27 )! !
!ShellDragDropSample class categoriesFor: #aboutText!enquiries!public! !
!ShellDragDropSample class categoriesFor: #resource_Default_view!public!resources-views! !

