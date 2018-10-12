"Filed out from Dolphin Smalltalk 7"!

View subclass: #USDoubleBufferedView
	instanceVariableNames: 'backSurface requiresRender'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
USDoubleBufferedView guid: (GUID fromString: '{54b0b57f-7902-4ea2-a3ea-5cc81ca16b52}')!
USDoubleBufferedView comment: 'This is a slightly changed DoubleBuffered View which does not draw directly on the Views DC but instead always uses the WM_PAINT provided DC.
'!
!USDoubleBufferedView categoriesForClass!Unclassified! !
!USDoubleBufferedView methodsFor!

canvas
	"Answer a <Canvas> onto the back surface"

	^(backSurface canvas)
		!

flipTo: aCanvas 
	"Private - Flip the current back surface to the front and paint it"

	backSurface 
		drawOn: aCanvas
		at: Point zero
		extent: backSurface extent!

graphics
	"Answer a <GdiplusGraphic> onto the back surface"

	^GdiplusGraphics fromCanvas: self canvas!

initialize
	"Private - Initialise the receiver."

	super initialize.
	backcolor := Color white.
	requiresRender := false.
!

initializeSurfacesFor: aPointExtent
	"Private - Initialize the front and back surfaces for a view size of aPointExtent"

	| canvas |

	backSurface notNil ifTrue: [ backSurface free ].
	canvas := super canvas.
	backSurface := Bitmap compatible: canvas extent: aPointExtent.
	self invalidate.
!

invalidate
	"Flag the current rendition as being invalid. A repaint will cause a
	render to occur"

	requiresRender := true.
	super invalidate!

onCreated: anEvent
	"Private - Handler for view created "

	super onCreated: anEvent.
	self initializeSurfacesFor: self extent.
!

onEraseRequired: aColorEvent
	"Private - Handler for erase background"

	^true!

onPaintRequired: aPaintEvent 
	"Private - Handler for paint event"

	requiresRender ifTrue: [self render].
	self flipTo:aPaintEvent canvas!

onPositionChanged: aPositionEvent
	"Private - Handle a window position change event (move or resize)."

	aPositionEvent isResize ifTrue: [
		self initializeSurfacesFor: aPositionEvent extent.
		self repaint ].
	^super onPositionChanged: aPositionEvent!

refreshContents
	"The model held by the receiver has been changed so repaint" 

	self repaint
!

render
	"Private - Render the background image"

	requiresRender := false
!

repaint
	"Repaints the receiver"

	self
		render; flipTo: super canvas! !
!USDoubleBufferedView categoriesFor: #canvas!accessing!public! !
!USDoubleBufferedView categoriesFor: #flipTo:!operations!private! !
!USDoubleBufferedView categoriesFor: #graphics!accessing!public! !
!USDoubleBufferedView categoriesFor: #initialize!initializing!private! !
!USDoubleBufferedView categoriesFor: #initializeSurfacesFor:!initializing!private! !
!USDoubleBufferedView categoriesFor: #invalidate!operations!public! !
!USDoubleBufferedView categoriesFor: #onCreated:!event handling!private! !
!USDoubleBufferedView categoriesFor: #onEraseRequired:!event handling!private! !
!USDoubleBufferedView categoriesFor: #onPaintRequired:!event handling!private! !
!USDoubleBufferedView categoriesFor: #onPositionChanged:!event handling!private! !
!USDoubleBufferedView categoriesFor: #refreshContents!public!updating! !
!USDoubleBufferedView categoriesFor: #render!operations!private! !
!USDoubleBufferedView categoriesFor: #repaint!operations!public! !

