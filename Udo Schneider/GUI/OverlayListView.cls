"Filed out from Dolphin Smalltalk 7"!

ListView subclass: #OverlayListView
	instanceVariableNames: 'getOverlayBlock overlayImageMapping'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OverlayListView guid: (GUID fromString: '{e8ad61d4-7107-4a85-b6a2-9997d54d5b7a}')!
OverlayListView comment: ''!
!OverlayListView categoriesForClass!Unclassified! !
!OverlayListView methodsFor!

addOrUpdate: aBoolean nonVirtualItems: aSequenceableCollection afterIndex: anInteger
	"Private - Add/update row and column items for specified sequence of objects to the ListView,
	starting at the specified row index."

	| lvItem primaryImageSupplier primaryTextSupplier isMultiColumn colImageBlock colOverlayBlock msg |
	primaryImageSupplier := primaryTextSupplier := self.
	colImageBlock := [:eachRow :eachCol | nil].
	(isMultiColumn := self isReportMode)
		ifTrue: 
			[primaryTextSupplier := self allColumns at: 1 ifAbsent: [self].
			self hasColumnImages
				ifTrue: 
					[colImageBlock := [:eachRow :eachCol | eachCol imageFromRow: eachRow].
					colOverlayBlock := [:eachRow :eachCol | eachCol overlayFromRow: eachRow].
					primaryImageSupplier := primaryTextSupplier]].
	lvItem := LVITEMW new.
	msg := aBoolean ifTrue: [LVM_SETITEMW] ifFalse: [LVM_INSERTITEMW].
	aSequenceableCollection keysAndValuesDo: 
			[:i :each |
			lvItem
				row: i + anInteger
				text: (primaryTextSupplier textFromRow: each)
				imageIndex: (self imageManager indexOfImage: (primaryImageSupplier imageFromRow: each))
				overlayIndex: (self imageManager indexOfOverlayImage: (primaryImageSupplier overlayFromRow: each))
				indent: (self indentFromRow: each).
			self
				sendMessage: msg
				wParam: 0
				lpParam: lvItem].
	isMultiColumn ifFalse: [^self].
	self allColumns from: 2
		keysAndValuesDo: 
			[:j :eachCol |
			lvItem column: j.
			aSequenceableCollection keysAndValuesDo: 
					[:i :eachRow |
					lvItem
						row: i + anInteger
						text: (eachCol textFromRow: eachRow)
						imageIndex: (self imageManager indexOfImage: (colImageBlock value: eachRow value: eachCol))
						overlayIndex: (self imageManager indexOfOverlayImage: (colOverlayBlock value: eachRow value: eachCol))
						indent: nil.
					self lvmSetItem: lvItem]]!

defaultGetImageBlock
	"Private - Answer a monadic valuable to use to find out the image index of an object 
	that will be placed in the receiver."

	^nil!

getOverlayBlock
	^getOverlayBlock!

getOverlayBlock: anObject 
	getOverlayBlock := anObject.
	self updateAll!

initialize
	"Private - Initialize the receiver"

	super initialize.
	getOverlayBlock := nil.
	imageManager := OverlayImageManager new!

onDisplayDetailsRequired: lvitem
	"Private - Get the display info for the receiver's row identified by the <LVITEM>, lvitem."

	"N.B. This is a callback request from the ListView so setting a breakpoint in here may bring
	your image to its knees."

	"Implementation Note: If in report mode then the task of supplying the text/images is
	delegated to the particular column, otherwise the valuables local to the receiver are used.
	This may seem inconsistent, but it allows different text/images to be displayed for the
	primary column if the application requires that the view be dynamically switchable between
	#report mode and the other modes."

	| rowObject mask column columnIdx overlayIdx |
	rowObject := self objectFromHandle: lvitem handle ifAbsent: [].
	"List sometimes asks for lvitem we no longer have, answer nil to accept default processing"
	rowObject isNil ifTrue: [^nil].
	self isReportMode
		ifTrue: 
			[columnIdx := lvitem iSubItem + 1.
			column := self columnAtIndex: columnIdx].
	mask := lvitem mask.

	"Image Request?"
	(mask allMask: LVIF_IMAGE)
		ifTrue: 
			[| primaryImageSupplier imgIdx |
			primaryImageSupplier := (column notNil and: [self hasColumnImages]) ifTrue: [column] ifFalse: [self].
			(primaryImageSupplier imageFromRow: rowObject)
				ifNotNil: 
					[:img |
					imgIdx := self imageManager indexOfImage: img.
					imgIdx notNil ifTrue: [lvitem image: imgIdx - 1]].
			(primaryImageSupplier overlayFromRow: rowObject)
				ifNotNil: 
					[:overlay |
					overlayIdx := self imageManager indexOfOverlayImage: overlay.
					overlayIdx notNil
						ifTrue: 
							[lvitem overlayImageIndex:  overlayIdx
							]]].

	"Text request?"
	(mask allMask: LVIF_TEXT)
		ifTrue: 
			["If in report mode the column's get text block is used unless the request
			 is for the primary column and its text block is nil, in which case the view
			 level block is used"
			lvitem
				textInBuffer: (((column notNil and: [columnIdx > 1 or: [column getTextBlock notNil]])
						ifTrue: [column]
						ifFalse: [self]) textFromRow: rowObject)].
	(mask allMask: LVIF_INDENT)
		ifTrue: 
			["Indenting is only supported for the whole row, not on a per-column basis"
			lvitem indent: (self indentFromRow: rowObject)].
	^0	"suppress default processing"!

overlayFromRow: item 
	^getOverlayBlock isNil ifFalse: [getOverlayBlock value: item]! !
!OverlayListView categoriesFor: #addOrUpdate:nonVirtualItems:afterIndex:!private!updating! !
!OverlayListView categoriesFor: #defaultGetImageBlock!adapters!constants!private! !
!OverlayListView categoriesFor: #getOverlayBlock!accessing!public! !
!OverlayListView categoriesFor: #getOverlayBlock:!accessing!public! !
!OverlayListView categoriesFor: #initialize!initializing!private! !
!OverlayListView categoriesFor: #onDisplayDetailsRequired:!event handling!private! !
!OverlayListView categoriesFor: #overlayFromRow:!private! !

!OverlayListView class methodsFor!

columnClass
	"Answer the class of object used to represent the columns of the receiver."

	^OverlayListViewColumn!

example1
	| listModel presenter view |
	"
		self example1
	"
	listModel := ListModel on: (1 to: 100).
	presenter := ListPresenter show: 'Overlay list view' on: listModel.
	view := presenter view.
	view viewMode: #tileIcons.
	"You should now see a ListPresenter showing all number between 1-100 w/o any icons"
	view getImageBlock: [:each | each icon].
	"Now each entry has a number icon.
		IMPORTANT: Please note that OverlayListPresenters do expect the getImageBlock to return the Icon itself - not the imageIndex of it!!!!!!"
	view getOverlayBlock: [:each | each even icon]
	"Now all number will have an overlay icon depending on whether they are even or not.
		IMPORTANT: Here as well you have to provide the icon itself - not it's imageIndex"!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	^(super publishedAspectsOfInstances)
		add: (Aspect block: #getOverlayBlock);
		yourself! !
!OverlayListView class categoriesFor: #columnClass!constants!private! !
!OverlayListView class categoriesFor: #example1!*-in class package!must strip!public! !
!OverlayListView class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !

