| package |
package := Package name: 'DH Shell Explorer'.
package paxVersion: 0;
	basicComment: 'Shell Explorer.

Copyright (c) Louis Sumberg and Steve Waring 2002.
	<lsumberg@mindspring.com>, <http://www.mindspring.com/~lsumberg/dolphin>
	<swaring@ozemail.com.au>, <http://www.dolphinharbor.org>
Public Domain Freeware.

This package contains a sample shell which replicates some of the Windows Explorer functionality.

	ShellExplorer show
	ShellExplorer show selectFolder: ShellFolder myComputer'.

package basicPackageVersion: '5.1.2.16'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAxIEYPDQAEAAAASW1hZ2VTdHJpcHBlcgAAAAAAAAAAUgAAABEAAABESCBTaGVsbCBFeHBs
b3JlclIAAAApAAAARG9scGhpbkhhcmJvclxTaGVsbFxESCBTaGVsbCBFeHBsb3Jlci5leGWaAAAA
AAAAALABAABSAAAAGwAAAFNoZWxsRXhwbG9yZXJTZXNzaW9uTWFuYWdlcu+/JwAAAAAABgMPAFZl
cnNpb25SZXNvdXJjZQAAAAAGARAAVlNfRklYRURGSUxFSU5GTwAAAAByAAAANAAAAL0E7/4AAAEA
AAABAAIAAAAAAAEAAgAAAD8AAAAAAAAABAAAAAIAAAAAAAAAAAAAAAAAAADqAAAAAAAAAPAAAABi
AAAAAgAAAFIAAAAIAAAAMDQwOTA0YjDqAAAAAAAAAPAAAABiAAAAGAAAAFIAAAAOAAAAUHJvZHVj
dFZlcnNpb25SAAAACgAAADEsIDAsIDAsIDJSAAAACwAAAENvbXBhbnlOYW1lUgAAAB4AAABTdGV2
ZSBXYXJpbmcgYW5kIExvdWlzIFN1bWJlcmdSAAAADAAAAFByaXZhdGVCdWlsZFIAAAAAAAAAUgAA
AAwAAABTcGVjaWFsQnVpbGRSAAAAAAAAAFIAAAAPAAAARmlsZURlc2NyaXB0aW9uUgAAABMAAABX
aW5kb3dzU2hlbGwgU2FtcGxlUgAAAA8AAABMZWdhbFRyYWRlbWFya3NSAAAAMQAAAERvbHBoaW4g
aXMgYSB0cmFkZW1hcmsgb2YgQ0dJIEdyb3VwIChFdXJvcGUpIEx0ZC5SAAAADAAAAEludGVybmFs
TmFtZVIAAAAAAAAAUgAAABAAAABPcmlnaW5hbEZpbGVuYW1lUgAAAA8AAABTaGVsbFNhbXBsZS5l
eGVSAAAADgAAAExlZ2FsQ29weXJpZ2h0UgAAACsAAABQb3J0aW9ucyBDb3B5cmlnaHQgqSBPYmpl
Y3QgQXJ0cyAxOTk3LTIwMDIuUgAAAAgAAABDb21tZW50c1IAAABPAAAAQSBEb2xwaGluIFRvIEdv
IEFwcGxpY2F0aW9uLiBQb3dlcmVkIGJ5IERvbHBoaW4gU21hbGx0YWxrIGFuZCBTcHJheSBXZWJz
ZXJ2aWNlc1IAAAALAAAARmlsZVZlcnNpb25SAAAACgAAADEsIDAsIDAsIDJSAAAACwAAAFByb2R1
Y3ROYW1lIAMAAMoAAAAAAAAA0AAAAGIAAAABAAAABgIKAERXT1JEQXJyYXkAAAAAcgAAAAQAAAAJ
BLAEAwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA').

package classNames
	add: #ShellExplorer;
	add: #ShellExplorerSessionManager;
	add: #ShellTreeModel;
	add: #ShellTreePresenter;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #ShellExplorer -> 'Default view';
	add: #ShellTreePresenter -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'DH Shell Core';
	add: 'DH Shell Notifications';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\MVP\Models\Tree\Dolphin Tree Models';
	add: '..\..\Object Arts\Dolphin\Lagoon\Lagoon Image Stripper';
	yourself).

package!

"Class Definitions"!

VirtualTreeModel subclass: #ShellTreeModel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TreePresenter subclass: #ShellTreePresenter
	instanceVariableNames: 'shellNotification notifyTime'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #ShellExplorer
	instanceVariableNames: 'folders subObjects shellNotification'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RuntimeSessionManager subclass: #ShellExplorerSessionManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

ShellTreeModel guid: (GUID fromString: '{70863661-7564-4AF7-B119-A9E4D572C2C1}')!
ShellTreeModel comment: 'ShellTreeModel represents all or a portion of the Shell Namespace.  The root is typically the Desktop ShellFolder, though multiple roots are permitted and any root may be at a level other than the Desktop.  '!
!ShellTreeModel categoriesForClass!Unclassified! !
!ShellTreeModel methodsFor!

approxSize
	"Private - Answer the approximate size of the receiver.  Note: It doesn't have to be accurate but it should be fast."

	^500!

searchPolicy
	"Answer the <searchPolicy> used to compare and search for elements by the receiver.
	Implementation Note: As we are fronting an externally represented tree (i.e the Shell Namespace)
	the nodes are created to represent the external keys and values, and are not, therefore, identical."

	^SearchPolicy equality!

setRoots: aCollection
	"Private - Initialize the receiver with the roots in aCollection.  Deliberately omit super initialize"

	self basicRoots: aCollection.
	hasChildrenBlock := [:so | so isValid and: [so hasSubFolders]].
	getChildrenBlock := [:so | so isValid ifTrue: [so subFolders asSortedCollection] ifFalse: [#()]].
	getParentBlock := [:so | so isValid ifTrue: [so parent] ifFalse: [nil]]! !
!ShellTreeModel categoriesFor: #approxSize!accessing!private! !
!ShellTreeModel categoriesFor: #searchPolicy!accessing!public! !
!ShellTreeModel categoriesFor: #setRoots:!initializing!private! !

ShellTreePresenter guid: (GUID fromString: '{DF444F8C-4B77-42C6-82C6-E90C3E39635D}')!
ShellTreePresenter comment: 'ShellTreePresenter is a TreePresenter specialized to deal with ShellFolders in a ShellTreeModel.  ShellTreePresenter adds event notifications for external changes in the Shell Namespace and uses the system icons in the TreeView alongside each ShellFolder node.  ShellTreePresenter also offers a non-modal GUI alternative to BrowseFolderDialog.

	ShellTreePresenter show.'!
!ShellTreePresenter categoriesForClass!Unclassified! !
!ShellTreePresenter methodsFor!

iconIndexFor: aShellObject
	"This is the behavior I get in Windows Explorer"

	^(self selectionOrNil == aShellObject 
		ifTrue: [aShellObject sysSmallOpenIconIndex]
		ifFalse: [aShellObject sysSmallIconIndex]) + 1!

isExpanded: anObject
	"Answer whether or not the <Object> argument is an expanded item in the tree.
	This method should really be in TreeView.  TVIF_STATE = 8, TVIS_EXPANDED = 32"

	| tvItem |
	(tvItem := TVITEM new)
		mask: 8;
		hItem: (self view handleFromObject: anObject);
		stateMask: 32.
	self view tvmGetItem: tvItem.
	^(tvItem dwState bitAnd: 32) = 32!

model: aTreeModel 
	super model: aTreeModel.
	shellNotification := aTreeModel roots first notificationForFolderTree.
	shellNotification 
		when: #shellChanged:eventId:
		send: #onShellChanged
		to: self!

onShellChanged
	"Something about the folder structure has changed."

	Time now asMilliseconds - notifyTime > 500 ifTrue: [self refresh].
	notifyTime := Time now asMilliseconds!

onViewClosed
	"Sent by the receiver's view when it has been closed.
	Disconnect from any events triggered "

	super onViewClosed.
	self shellNotifyOff!

onViewOpened
	"Direct the listview to retrieve ShellObject icons."

	super onViewOpened.
	notifyTime := Time now asMilliseconds.
	(self view)
		imageManager: SystemImageManager current;
		getImageBlock: [:o | self iconIndexFor: o];
		hasLinesAtRoot: false!

refresh
	"Refresh the receiver's view, and restore the selected node and its expansion state."

	| folder expanded newSelection |
	folder := self selectionOrNil.
	(folder notNil and: [folder isValid]) ifTrue: [newSelection := folder siblingOrParent].
	expanded := self view displayedContents select: [:e | self isExpanded: e].
	self view refreshContents.
	expanded do: 
			[:obj | 
			obj allParents do: [:e | self expand: e].
			self expand: obj].
	(folder notNil and: [folder refresh isValid]) 
		ifTrue: [self selection: folder]
		ifFalse: 
			[(newSelection notNil and: [newSelection refresh isValid]) 
				ifTrue: [self selection: newSelection]]!

selectFolder: aShellFolder 
	self selectFolder: aShellFolder expand: true!

selectFolder: aShellFolder expand: aBoolean 
	aShellFolder isValid 
		ifTrue: 
			[self selection: aShellFolder.
			aBoolean ifTrue: [self expand: aShellFolder]]!

shellNotifyOff
	shellNotification ifNotNil: [:object | object removeEventsTriggeredFor: self]! !
!ShellTreePresenter categoriesFor: #iconIndexFor:!accessing!public! !
!ShellTreePresenter categoriesFor: #isExpanded:!public!testing! !
!ShellTreePresenter categoriesFor: #model:!initializing!public! !
!ShellTreePresenter categoriesFor: #onShellChanged!event handling!public! !
!ShellTreePresenter categoriesFor: #onViewClosed!event handling!public! !
!ShellTreePresenter categoriesFor: #onViewOpened!event handling!public! !
!ShellTreePresenter categoriesFor: #refresh!commands!public! !
!ShellTreePresenter categoriesFor: #selectFolder:!event handling!public! !
!ShellTreePresenter categoriesFor: #selectFolder:expand:!event handling!public! !
!ShellTreePresenter categoriesFor: #shellNotifyOff!commands!public! !

!ShellTreePresenter class methodsFor!

defaultModel
	^ShellTreeModel withRoots: (Array with: (ShellFolder desktop isDisplayEnumErrors: true))! !
!ShellTreePresenter class categoriesFor: #defaultModel!models!public! !

ShellExplorer guid: (GUID fromString: '{D986FD06-0A03-40FE-B9F2-CA43AE8412DB}')!
ShellExplorer comment: 'ShellExplorer is a simplified version of WIndows Explorer.  It demonstrates the use of ShellFolders and ShellFiles and  ShellNotifications.

	ShellExplorer show.'!
!ShellExplorer categoriesForClass!Unclassified! !
!ShellExplorer methodsFor!

createComponents
	"Create the presenters contained by the receiver. At this stage the receiver has not yet been initialized."

	super createComponents.
	folders := self add: ShellTreePresenter new name: 'folders'.
	subObjects := self add: ListPresenter new name: 'subObjects'!

createSchematicWiring
	"Create the trigger wiring for the receiver. At this stage the initialization is complete and the view is open"

	super createSchematicWiring.
	folders 
		when: #selectionChanged
		send: #onFolderSelectionChanged
		to: self.
	subObjects 
		when: #actionPerformed
		send: #openSelectedSubObjects
		to: self!

deleteFolder: aShellFolder
	(MessageBox confirm: 'Are you sure you want to delete this folder?'
		caption: 'Confirm deletion') ifTrue: [aShellFolder delete]!

deleteSelectedFolder
	| selection newSelection |
	selection := folders selection.
	newSelection := selection siblingOrParent.
	self deleteFolder: selection.
	self selectFolder: newSelection!

deleteSelectedSubObjects
	self deleteSubObjects: subObjects selections!

deleteSubObjects: aCollection
	aCollection do: [:e | e first delete]!

expandSelectedFolder
	| selection |
	((selection := folders selectionOrNil) isNil or: [selection isValid not]) ifTrue: [^nil].
	folders expand: selection!

helpAbout
	(MessageBox new)
		caption: 'About ' , self class name asString;
		icon: self class icon;
		text: self class aboutText;
		open.
	self view lastFocus setFocus!

isDeletable: aShellObject
	"Answer whether aShellObject should be deleted if requested by the user."

	aShellObject isDesktop ifTrue: [^false].
	aShellObject isFileSystem not ifTrue: [^false].
	aShellObject isFolder 
		ifTrue: 
			[aShellObject parent isDesktop 
				ifTrue: [^false]
				ifFalse: [aShellObject parent parent isDesktop ifTrue: [^false]]].
	^true!

model: aShellTreeModel
	"Set the model of the receiver to be aShellTreeModel."

	super model: aShellTreeModel.
	folders model: aShellTreeModel!

onDeferredStartup
	self model: ShellTreePresenter defaultModel.
	folders selectFolder: self model roots first!

onFolderSelectionChanged
	"The selected folder has changed.  If not nil, update the file list."

	| selection |
	selection := folders selectionOrNil.
	self updateListFor: selection.
	self updateCaption!

onStartup
	SessionManager current inputState queueDeferredAction: [self onDeferredStartup]!

onViewClosed
	"Sent by the receiver's view when it has been closed.  Disconnect from any events triggered "

	super onViewClosed.
	shellNotification ifNotNil: [:object | object removeEventsTriggeredFor: self]!

onViewOpened
	"Direct the listview to retrieve ShellObject icons.  Select and expand the My Computer folder."

	super onViewOpened.
	(subObjects view)
		imageManager: SystemImageManager current;
		getImageBlock: [:arr | arr first sysSmallIconIndex + 1]!

openSelectedSubObjects
	"If selection in files list is a folder, then select it in the folders list, else open the file."

	| selections |
	(selections := subObjects selections collect: [:e | e first]) do: 
			[:obj | 
			(folders selection subFolders includes: obj) 
				ifTrue: 
					[selections size > 1 
						ifTrue: [(self class create selectFolder: obj) show]
						ifFalse: 
							[self expandSelectedFolder.
							folders selection: obj]]
				ifFalse: [ShellLibrary default shellOpen: obj displayParsingName directory: nil]]!

queryCommand: query
	"Private - Enters details about a potential command for the receiver into the <CommandQuery>,  query."

	| command |
	command := query command.
	#deleteSelectedFolder == command 
		ifTrue: 
			[query isEnabled: (self isDeletable: folders selection).
			^true].
	#deleteSelectedSubObjects == command 
		ifTrue: 
			[query isEnabled: (subObjects selections anySatisfy: [:e | self isDeletable: e first]).
			^true].
	^super queryCommand: query!

selectFolder: aShellFolder 
	(folders model includes: aShellFolder) ifTrue: [folders selectFolder: aShellFolder]!

status: aString
	"Private - Display the <readableString> aString in the receiver's status area."

	((self view viewNamed: 'statusBar') itemAtIndex: 1) model: aString asValue!

updateCaption
	"Update the shell's caption based on the currently selected folder."

	| folder text |
	folder := folders selectionOrNil.
	text := (folder isNil or: [folder isValid not]) 
				ifTrue: ['']
				ifFalse: 
					[folder isFileSystem 
						ifTrue: ['- ' , folder displayParsingName]
						ifFalse: ['- ' , folder displayName]].
	self caption: 'Exploring ' , text!

updateListFor: aSelectedFolder 
	| col |
	col := OrderedCollection new.
	(aSelectedFolder notNil and: [aSelectedFolder refresh isValid]) 
		ifTrue: 
			[col := (aSelectedFolder subObjects collect: 
							[:e | 
							Array 
								with: e
								with: e fileSize
								with: e typeName
								with: e lastWriteTime]) 
						asSortedCollection: (subObjects isSorted 
								ifTrue: [subObjects sortBlock]
								ifFalse: [[:a :b | a first <= b first]])].
	subObjects model list: col.
	self updateStatus!

updateStatus
	| list total str |
	list := subObjects model.
	total := 0.
	list do: [:arr | total := total + (arr second ifNil: [0] ifNotNil: [:fs | fs])].
	str := total > 1000000 
				ifTrue: [(total / 1048576) asInteger displayString , ' MB']
				ifFalse: [(total / 1024) asInteger displayString , ' KB'].
	self status: list size displayString , ' object(s)  ' , str! !
!ShellExplorer categoriesFor: #createComponents!initializing!public! !
!ShellExplorer categoriesFor: #createSchematicWiring!initializing!public! !
!ShellExplorer categoriesFor: #deleteFolder:!operations!public! !
!ShellExplorer categoriesFor: #deleteSelectedFolder!commands!public! !
!ShellExplorer categoriesFor: #deleteSelectedSubObjects!commands!public! !
!ShellExplorer categoriesFor: #deleteSubObjects:!operations!public! !
!ShellExplorer categoriesFor: #expandSelectedFolder!operations!public! !
!ShellExplorer categoriesFor: #helpAbout!commands!public! !
!ShellExplorer categoriesFor: #isDeletable:!public!testing! !
!ShellExplorer categoriesFor: #model:!accessing!public! !
!ShellExplorer categoriesFor: #onDeferredStartup!event handling!initializing!public! !
!ShellExplorer categoriesFor: #onFolderSelectionChanged!event handling!public! !
!ShellExplorer categoriesFor: #onStartup!event handling!initializing!public! !
!ShellExplorer categoriesFor: #onViewClosed!event handling!private! !
!ShellExplorer categoriesFor: #onViewOpened!event handling!initializing!public! !
!ShellExplorer categoriesFor: #openSelectedSubObjects!event handling!public! !
!ShellExplorer categoriesFor: #queryCommand:!commands!private! !
!ShellExplorer categoriesFor: #selectFolder:!public!selection! !
!ShellExplorer categoriesFor: #status:!accessing!private! !
!ShellExplorer categoriesFor: #updateCaption!public!updating! !
!ShellExplorer categoriesFor: #updateListFor:!public!updating! !
!ShellExplorer categoriesFor: #updateStatus!public!updating! !

!ShellExplorer class methodsFor!

aboutText
	^'Shell Explorer Sample

Copyright: 2002 
  Louis Sumberg <lsumberg@mindspring.com>
  Steve Waring <swaring@ozemail.com.au>

Version: %1 

DISCLAIMER: This software is freely provided purely as a sample and as such it
is provided "as is", WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE' 
		formatWith: SessionManager current versionInfo productVersionString!

defaultModel
	"Answer a default model to be assigned to the receiver when it is initialized."

	^ShellTreePresenter defaultModel!

forcePrereq
	"Force a prereq to the 'Shell Notifications' package
	(Could use manual prerequisites, but this is easier to maintain)"

	^ShellNotification!

icon
	^Icon fromId: 46 in: ShellLibrary default
!

initialize
	"	self initialize	"

	SessionManager current 
		when: #sessionStarted
		send: #onStartup
		to: self!

onStartup
	"Private - Ensure all the receiver's subinstances are in their clean state on startup
	rather than attempting to use an old safe array hanging around from the sesssion 
	when the image was saved."

	self primAllSubinstances do: [:i | i onStartup]! !
!ShellExplorer class categoriesFor: #aboutText!enquiries!public! !
!ShellExplorer class categoriesFor: #defaultModel!models!public! !
!ShellExplorer class categoriesFor: #forcePrereq!helpers!public! !
!ShellExplorer class categoriesFor: #icon!constants!public! !
!ShellExplorer class categoriesFor: #initialize!initializing!public! !
!ShellExplorer class categoriesFor: #onStartup!event handling!private! !

ShellExplorerSessionManager guid: (GUID fromString: '{D1248D37-13FE-4FF4-A7B1-8B7AFE39B688}')!
ShellExplorerSessionManager comment: ''!
!ShellExplorerSessionManager categoriesForClass!Unclassified! !
!ShellExplorerSessionManager methodsFor!

main
	self mainShellClass show! !
!ShellExplorerSessionManager categoriesFor: #main!operations-startup!public! !

!ShellExplorerSessionManager class methodsFor!

mainShellClass
	"Answer the class of the application's main window (a <Shell> presenter)."

	^ShellExplorer!

updateVersionResource: versionResource
	"ShellExplorerSessionManager updateVersionResource: self versionResource"

	| versionString description |
	versionString := '1,0,0,2'.
	description := 'WindowsShell Sample'.
	versionResource
		fileVersion: versionString;
		productVersion: versionString.
	(versionResource stringTables values first)
		at: 'CompanyName' put: 'Steve Waring and Louis Sumberg';
		at: 'FileDescription' put: description;
		at: 'Comments'
			put: 'A Dolphin To Go Application. Powered by Dolphin Smalltalk and Spray Webservices';
		at: 'ProductName' put: description;
		at: 'OriginalFilename' put: 'ShellSample.exe'! !
!ShellExplorerSessionManager class categoriesFor: #mainShellClass!constants!public! !
!ShellExplorerSessionManager class categoriesFor: #updateVersionResource:!helpers!public! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: ShellExplorer name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAIIYAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAACQAAAFNoZWxsVmlld2IAAAAbAAAA
AAAAAAAAAABiAAAAAgAAAAEAngEBAAIAoAEAAAAAAAAAAAAAAAAAAAcCAAAAAAAAAAAAAAAAAACg
AQAABgcMAEJvcmRlckxheW91dAAAAAABAAAAAQAAAAAAAACaAQAAAAAAAJoAAAAAAAAAUgAAABQA
AABEb2xwaGluIENvbnRyb2wgQmFyc1IAAAAJAAAAU3RhdHVzQmFyYgAAABIAAAAAAAAAoAEAAGIA
AAACAAAAggAAAAQAAAAECQBEAQAAACACAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAYEBABGb250AAAA
AAAAAAAQAAAABgEHAExPR0ZPTlQAAAAAcgAAADwAAAD1////AAAAAAAAAAAAAAAAkAEAAAAAAAAD
AgEiTWljcm9zb2Z0IFNhbnMgU2VyaWYAOwAAAAD/AAVWdwEGAgUAUG9pbnQAAAAAwQAAAMEAAAAA
AAAAIAIAAAAAAACCAAAACAAAAJkD//8AAAAA6gAAAAAAAAAAAQAAYgAAAAAAAABiAAAAAQAAAAYH
DQBTdGF0dXNCYXJJdGVtAAAAAAEAAAD/////IAIAAAAAAACaAAAAAAAAAMABAABSAAAAEQAAAEJh
c2ljTGlzdEFic3RyYWN0AAAAAA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAAMABAABS
AAAAEAAAAEljb25JbWFnZU1hbmFnZXK6AAAAAAAAAFIAAAAHAAAAY3VycmVudAYEEQBTdGF0dXNC
YXJOdWxsSXRlbQAAAAABAgAAAQAAACACAAAAAAAAAAAAAAYBDwBNZXNzYWdlU2VxdWVuY2UAAAAA
ygAAAAAAAADQAAAAYgAAAAEAAAAGAwsATWVzc2FnZVNlbmQAAAAAugAAAAAAAABSAAAAEAAAAGNy
ZWF0ZUF0OmV4dGVudDpiAAAAAgAAAOICAAAAAAAAAQAAAGkBAADiAgAAAAAAAJUEAAApAAAAIAIA
AAYBDwBXSU5ET1dQTEFDRU1FTlQAAAAAcgAAACwAAAAsAAAAAAAAAAEAAAD/////////////////
////AAAAALQAAABKAgAAyAAAAMoAAAAAAAAA0AAAACADAADiAgAAAAAAAMEAAADBAAAAAAAAABMA
AAAAAAAAAAAAAJoBAAAAAAAAmgAAAAAAAADAAQAAUgAAAA0AAABDb250YWluZXJWaWV3YgAAAA8A
AAAAAAAAoAEAAGIAAAACAAAAggAAAAQAAAAAAABEAQACAAAFAAAAAAAAAAAAAAAAAAAHAAAAAAAA
AAAAAAAAAAAAAAUAAAYCEgBQcm9wb3J0aW9uYWxMYXlvdXQAAAAA6gAAAAAAAADwAAAAYgAAAAIA
AACaAQAAAAAAAJoAAAAAAAAAUgAAABcAAABEb2xwaGluIENvbW1vbiBDb250cm9sc1IAAAAZAAAA
TXVsdGlwbGVTZWxlY3Rpb25MaXN0Vmlld2IAAAAeAAAAAAAAAAAFAABiAAAAAgAAAIIAAAAEAAAA
SRABRAEEAACgBQAARgMJAAIAAABMaXN0TW9kZWwAAAAAygAAAAAAAADQAAAAIAMAAAAAAACKAwAA
AAAAAJoAAAAAAAAAUgAAAAcAAABEb2xwaGluUgAAAAwAAABTZWFyY2hQb2xpY3m6AAAAAAAAAFIA
AAAIAAAAaWRlbnRpdHkAAAAAAAAAAAcAAABGBQQAAgAAAE1lbnUAAAAAAAAAABAAAABiAAAAAQAA
AEYEDwACAAAAQ29tbWFuZE1lbnVJdGVtAAAAAAEAAABGBRIABAAAAENvbW1hbmREZXNjcmlwdGlv
bgAAAAC6AAAAAAAAAFIAAAAYAAAAZGVsZXRlU2VsZWN0ZWRTdWJPYmplY3RzUgAAAAYAAABEZWxl
dGUBAAAAAQAAAAAAAAAAAAAAAAAAAFIAAAAAAAAAAAAAAJICAAAAAAAAAAAAABAAAACyAgAAAAAA
AHIAAAA8AAAA9f///wAAAAAAAAAAAAAAAJABAAAAAAAAAQIBIk1TIFNhbnMgU2VyaWYAzDVHAQUA
FDsAAAAA/wAFVncB4gIAAAAAAADBAAAAwQAAAAAAAACgBQAAAAAAAIIAAAAIAAAADwP//wAAAABg
AwAAmgAAAAAAAADABQAAUgAAABIAAABJY29uaWNMaXN0QWJzdHJhY3SQAwAAAAAAAAAAAAAAAAAA
4gIAAAAAAABBAAAAQQAAAAAAAAAAAAAAygAAAAAAAADQAAAAYgAAAAQAAABGDA4ABQAAAExpc3RW
aWV3Q29sdW1uAAAAAFIAAAAEAAAATmFtZS0BAAC6AAAAAAAAAFIAAAAEAAAAbGVmdGADAACaAAAA
AAAAAGAGAABSAAAAEAAAAFNvcnRlZENvbGxlY3Rpb24GBAwAQmxvY2tDbG9zdXJlAAAAACYDDQBN
ZXRob2RDb250ZXh0AQAAAAEAAAAmBRIAQ29tcGlsZWRFeHByZXNzaW9uAAAAAIEBAACaAAAAAAAA
AGAGAABSAAAADwAAAFVuZGVmaW5lZE9iamVjdFIAAAAEAAAAZG9JdGIAAAACAAAAUgAAABIAAABb
OmFyciB8IGFyciBhdDogMV1iAAAAAQAAAMoAAAAAAAAAmgAAAAAAAABgBgAAUgAAAA4AAABQb29s
RGljdGlvbmFyeSADAAByAAAACwAAAPsBBQBZET+UamRpAAAAAAAAAAADAAAACwAAALAIAAAAAAAA
oAUAAAAAAAABAAAAAAAAAAAAAADyBwAAAAAAAFIAAAAEAAAAU2l6ZZcAAAAgCAAAYggAAAAAAACC
CAAAAQAAAAEAAACiCAAAAAAAAIEBAADACAAAUgAAAAQAAABkb0l0YgAAAAIAAABSAAAAEgAAAFs6
YXJyIHwgYXJyIGF0OiAyXWIAAAABAAAAygAAAAAAAAAwCQAAIAMAAHIAAAALAAAA+wEFAFkRQJRq
ZGkAAAAAAAAAAAMAAAALAAAAoAkAAGIIAAAAAAAAgggAAAIAAAABAAAAoggAAAAAAACBAgAAwAgA
AFIAAAAEAAAAZG9JdGIAAAACAAAAUgAAAJ0AAABbOmEgOmIgfCAoKChhIGF0OiAyKSBub3ROaWwg
YW5kOiBbKGIgYXQ6IDIpIG5vdE5pbF0pIGFuZDogWyhhIGF0OiAyKSB+PSAoYiBhdDogMikgXSkN
CglpZlRydWU6IFsoYSBhdDogMikgPD0gKGIgYXQ6IDIpIF0NCglpZkZhbHNlOiBbKGEgYXQ6IDEp
IDw9IChiIGF0OiAxKV1dYgAAAAEAAADKAAAAAAAAADAJAAAgAwAAcgAAAC4AAAD7AigAWlkRQJTe
BRJAlJ1uO30RQJQSQJSFbjt9EUCUEkCUgmoRP5QSP5SCamRpAAAAAAAAAAAAAAAABQAAAAsAAAAw
CgAAAAAAAAAAAACgBQAAAAAAAAEAAAAAAAAAAAAAAPIHAAAAAAAAUgAAAAQAAABUeXBlyQAAACAI
AABiCAAAAAAAAIIIAAABAAAAAQAAAKIIAAAAAAAAgQEAAMAIAABSAAAABAAAAGRvSXRiAAAAAgAA
AFIAAAASAAAAWzphcnIgfCBhcnIgYXQ6IDNdYgAAAAEAAADKAAAAAAAAADAJAAAgAwAAcgAAAAwA
AAD7AQYAWRHWA5RqZGkAAAAAAAAAAAMAAAALAAAA4AoAAGIIAAAAAAAAgggAAAIAAAABAAAAoggA
AAAAAACBAgAAwAgAAFIAAAAEAAAAZG9JdGIAAAACAAAAUgAAAGcAAABbOmEgOmIgfCAoYSBhdDog
MykgPSAoYiBhdDogMykNCglpZlRydWU6IFsoYSAgYXQ6IDEpIDw9IChiIGF0OiAxKV0NCglpZkZh
bHNlOiBbKGEgYXQ6IDMpIDw9IChiIGF0OiAzKV1dYgAAAAEAAADKAAAAAAAAADAJAAAgAwAAcgAA
ACQAAAD7Ah4AWlkR1gOUEtYDlIR9ET+UEj+UgmoR1gOUEtYDlIJqZGkAAAAAAAAAAAAAAAAFAAAA
CwAAAHALAAAAAAAAAAAAAKAFAAAAAAAAAQAAAAAAAAAAAAAA8gcAAAAAAABSAAAACAAAAE1vZGlm
aWVkyQAAACAIAABiCAAAAAAAAIIIAAABAAAAAQAAAKIIAAAAAAAAgQEAAMAIAABSAAAABAAAAGRv
SXRiAAAAAgAAAFIAAAASAAAAWzphcnIgfCBhcnIgYXQ6IDRdYgAAAAEAAADKAAAAAAAAADAJAAAg
AwAAcgAAAAwAAAD7AQYAWRHWBJRqZGkAAAAAAAAAAAMAAAALAAAAIAwAAGIIAAAAAAAAgggAAAIA
AAABAAAAoggAAAIAAACBAgAAwAgAAFIAAAAEAAAAZG9JdGIAAAACAAAAUgAAAOMAAABbOmEgOmIg
fCAoKGEgYXQ6IDEpIGlzRm9sZGVyID0gKGIgYXQ6IDEpIGlzRm9sZGVyKSBpZlRydWU6IFsoKGEg
YXQ6IDQpIG5vdE5pbCBhbmQ6IFsoYiBhdDogNCkgbm90TmlsXSkNCglpZlRydWU6IFsoYSBhdDog
NCkgYXNJbnRlZ2VyIDw9IChiIGF0OiA0KSBhc0ludGVnZXJdDQoJaWZGYWxzZTogWyhhIGF0OiAx
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
////////////////////wgAAAAAAAADFAAAAtAAAAMoAAAAAAAAA0AAAACADAADwBAAAAAAAABMA
AACgBQAA8AQAAAAAAAATAAAA6gAAAAAAAAAAAQAAYgAAAAIAAAAgAgAAUgAAAAkAAABzdGF0dXNC
YXIAAAAARgUHAAIAAABNZW51QmFyAAAAAAAAAAAQAAAAYgAAAAMAAACiBgAAAAAAAAAAAAAQAAAA
YgAAAAEAAADSBgAAAAAAAAEAAADyBgAAAAAAALoAAAAAAAAAUgAAAAQAAABleGl0UgAAAAUAAABF
JnhpdAEAAAABAAAAAAAAAAAAAAAAAAAAUgAAAAUAAAAmRmlsZQAAAACiBgAAAAAAAAAAAAAQAAAA
YgAAAAEAAADSBgAAAAAAAAEAAADyBgAAAAAAAOAPAABSAAAACAAAACZSZWZyZXNo6QAAAAEAAAAA
AAAAAAAAAAAAAABSAAAABQAAACZWaWV3AAAAAKIGAAAAAAAAAAAAABAAAABiAAAAAQAAANIGAAAA
AAAAAQAAAPIGAAAAAAAAugAAAAAAAABSAAAACQAAAGhlbHBBYm91dFIAAAAFAAAAQWJvdXQBAAAA
AQAAAAAAAAAAAAAAAAAAAFIAAAAEAAAASGVscAAAAABSAAAAAAAAAAAAAAAAAAAABgMQAEFjY2Vs
ZXJhdG9yVGFibGUAAAAAAAAAABAAAABiAAAAAQAAAAYCCwBBc3NvY2lhdGlvbgAAAADpAAAAgBQA
AAAAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAIEAAAAAAAAygAAAAAAAADQAAAA
YgAAAAMAAABCBAAAAAAAAGAEAABiAAAAAgAAAOICAAAAAAAACwAAAAsAAADiAgAAAAAAAKUEAADH
AQAAoAEAAEIEAAAAAAAAUA4AAGIAAAABAAAAUgAAAA4AAABTaGVsbCBFeHBsb3JlcqABAABCBAAA
AAAAALoAAAAAAAAAUgAAAAgAAABtZW51QmFyOmIAAAABAAAAsBMAAKABAACyBAAAAAAAAHIAAAAs
AAAALAAAAAAAAAAAAAAA/////////////////////wUAAAAFAAAAVwIAAOgAAADKAAAAAAAAANAA
AABiAAAAAgAAAAAFAAAgAgAA8AQAAAAAAAAVAAAARgUEAAMAAABJY29uAAAAAAAAAAAQAAAADgIR
AFNUQlNpbmdsZXRvblByb3h5AAAAAJoAAAAAAAAAUgAAAAcAAABEb2xwaGluUgAAABgAAABJbWFn
ZVJlbGF0aXZlRmlsZUxvY2F0b3K6AAAAAAAAAFIAAAAHAAAAY3VycmVudFIAAAANAAAAU2hlbGxW
aWV3Lmljbw4CHwBTVEJFeHRlcm5hbFJlc291cmNlTGlicmFyeVByb3h5AAAAAFIAAAAQAAAAZG9s
cGhpbmRyMDA1LmRsbAAAAAA='))!

(ResourceIdentifier class: ShellTreePresenter name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAABYDAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAFwAAAERvbHBoaW4gQ29tbW9uIENvbnRyb2xzUgAAAAgAAABUcmVlVmlld2IA
AAAbAAAAAAAAAAAAAABiAAAAAgAAAIIAAAAEAAAAJwABRAEEAACgAQAARgQJAAIAAABUcmVlTW9k
ZWwAAAAAAAAAAAYDCABUcmVlTm9kZQAAAAAAAAAAAAAAAAAAAADqAAAAAAAAAAABAABiAAAAAAAA
AAYAFABJZGVudGl0eVNlYXJjaFBvbGljeQAAAAAAAAAAAAAAAAcAAAAAAAAAAAAAAAAAAACgAQAA
AAAAAIIAAAAIAAAAJQP//wAAAACaAAAAAAAAAFIAAAAQAAAARG9scGhpbiBNVlAgQmFzZVIAAAAR
AAAAQmFzaWNMaXN0QWJzdHJhY3SaAAAAAAAAAMABAABSAAAAEgAAAEljb25pY0xpc3RBYnN0cmFj
dA4CEQBTVEJTaW5nbGV0b25Qcm94eQAAAACaAAAAAAAAALACAABSAAAAEAAAAEljb25JbWFnZU1h
bmFnZXK6AAAAAAAAAFIAAAAHAAAAY3VycmVudAAAAAAAAAAAAAAAAAAAAAAAAAAA6gAAAAAAAADw
AAAAYAIAABEAAAC6AAAAAAAAAFIAAAAKAAAAc21hbGxJY29ucwEAAAAAAAAABgEPAE1lc3NhZ2VT
ZXF1ZW5jZQAAAADKAAAAAAAAANAAAABiAAAAAQAAAAYDCwBNZXNzYWdlU2VuZAAAAAC6AAAAAAAA
AFIAAAAQAAAAY3JlYXRlQXQ6ZXh0ZW50OmIAAAACAAAABgIFAFBvaW50AAAAAAsAAAALAAAAEgQA
AAAAAACRAQAAOwIAAKABAAAGAQ8AV0lORE9XUExBQ0VNRU5UAAAAAHIAAAAsAAAALAAAAAAAAAAA
AAAA/////////////////////wUAAAAFAAAAzQAAACIBAADKAAAAAAAAANAAAABgAgAAEgQAAAAA
AADBAAAAwQAAAAAAAAAXAAAARgUEAAMAAABJY29uAAAAAAAAAAAQAAAADgIRAFNUQlNpbmdsZXRv
blByb3h5AAAAAJoAAAAAAAAAUgAAAAcAAABEb2xwaGluUgAAABgAAABJbWFnZVJlbGF0aXZlRmls
ZUxvY2F0b3K6AAAAAAAAAFIAAAAHAAAAY3VycmVudFIAAAARAAAAVHJlZVByZXNlbnRlci5pY28O
Ah8AU1RCRXh0ZXJuYWxSZXNvdXJjZUxpYnJhcnlQcm94eQAAAABSAAAAEAAAAGRvbHBoaW5kcjAw
NS5kbGwAAAAA'))!

