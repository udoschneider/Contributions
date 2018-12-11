| package |
package := Package name: 'DH Shell Notifications'.
package paxVersion: 0;
	basicComment: 'Shell Notifications

Copyright (c) Louis Sumberg and Steve Waring 2002.
	<lsumberg@mindspring.com>, <http://www.mindspring.com/~lsumberg/dolphin>
	<swaring@ozemail.com.au>, <http://www.dolphinharbor.org>
Public Domain Freeware.

This package contains classes that enable objects to receive events from the Windows Shell.

Notes: 
 - I have read that under Win95, certain events are not signalled.
 - Events can be signalled twice. (This seems to be the standard in WinXP) .

Note:  This package uses undocumented windows functions.  See <http://www.geocities.com/SiliconValley/4942/notify.html>'.

package basicPackageVersion: '5.1.2.04'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAxIAAAAAA=').

package classNames
	add: #NOTIFYREGISTER;
	add: #ShellNotification;
	add: #ShellNotificationView;
	add: #ShellNotificationViewer;
	yourself.

package methodNames
	add: #ShellFolder -> #notificationForFiles;
	add: #ShellFolder -> #notificationForFolderTree;
	add: #ShellLibrary -> #SHChangeNotifyDeregister:;
	add: #ShellLibrary -> #SHChangeNotifyRegister:dwFlags:wEventMask:uMsg:cItems:lpItems:;
	yourself.

package globalNames
	add: #ShellNotificationConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	add: #ShellNotificationViewer -> 'Default view';
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'DH Shell Core';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls';
	add: '..\..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!

Win32Structure subclass: #NOTIFYREGISTER
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GraphicsTool subclass: #ShellNotification
	instanceVariableNames: 'parseName eventId isWatchSubtree'
	classVariableNames: ''
	poolDictionaries: 'ShellNotificationConstants'
	classInstanceVariableNames: ''!
Shell subclass: #ShellNotificationViewer
	instanceVariableNames: 'notificationsListPresenter eventListPresenter isCapture'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
View subclass: #ShellNotificationView
	instanceVariableNames: 'notifications'
	classVariableNames: 'DefaultInstance'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ShellFolder methodsFor!

notificationForFiles
	"Answer a <ShellNotification> on the receiver that monitors Shell changes for disk events.
	Note: We may need to capture an error creating this and answer nil"

	^(ShellNotification forShellObject: self) watchFiles!

notificationForFolderTree
	"Answer a registered ShellNotification on the receiver and its subFolders.
	Note: We may need to capture an error creating this and answer nil"

	^(ShellNotification forShellObject: self) watchFolders! !
!ShellFolder categoriesFor: #notificationForFiles!events!public! !
!ShellFolder categoriesFor: #notificationForFolderTree!events!public! !

!ShellLibrary methodsFor!

SHChangeNotifyDeregister: hNotify
	"Notify the Shell that we no longer want to be informed of changes from hNotify.  Answer whether successful or not.
		hNotify, a <ShellNotification> handle."

	<stdcall: handle 4 handle>
	^self invalidCall!

SHChangeNotifyRegister: hWnd dwFlags: dwFlags wEventMask: wEventMask uMsg: uMsg cItems: cItems lpItems: lpItems
	"Register a window with the shell, which will then be notified of all subsequent SHChangeNotify events.
	Answer the handle of a shell change notification object, or nil upon failure.
		hWnd, the handle of the window that should receive notifications.
		dwFlags, one or more SHCNE constants that filter out interrupt and non-interrupt events.
		wEventMask, one or more SHCNE constants that specify the events of interest.
		uMsg, the message that will be sent to the hWnd window.
		cItems, the number of items in the lpItems array.
		lpItems, an array of <NOTIFYREGISTER>s to monitor."

	"HANDLE WINAPI SHChangeNotifyRegister(
     	     HWND   hWnd, 
	     DWORD  dwFlags, 
	     LONG   wEventMask, 
	     UINT   uMsg, 
	     DWORD  cItems,
	     LPCNOTIFYREGISTER lpItems);"

	<stdcall: handle 2 handle dword dword dword dword NOTIFYREGISTER*>
	^self invalidCall! !
!ShellLibrary categoriesFor: #SHChangeNotifyDeregister:!public!win32 functions-shell library! !
!ShellLibrary categoriesFor: #SHChangeNotifyRegister:dwFlags:wEventMask:uMsg:cItems:lpItems:!public!win32 functions-shell library! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #ShellNotificationConstants put: (PoolConstantsDictionary named: #ShellNotificationConstants)!
ShellNotificationConstants at: 'SHCNE_ALLEVENTS' put: 16r7FFFFFFF!
ShellNotificationConstants at: 'SHCNE_ASSOCCHANGED' put: 16r8000000!
ShellNotificationConstants at: 'SHCNE_ATTRIBUTES' put: 16r800!
ShellNotificationConstants at: 'SHCNE_CREATE' put: 16r2!
ShellNotificationConstants at: 'SHCNE_DELETE' put: 16r4!
ShellNotificationConstants at: 'SHCNE_DISKEVENTS' put: 16r2381F!
ShellNotificationConstants at: 'SHCNE_DRIVEADD' put: 16r100!
ShellNotificationConstants at: 'SHCNE_DRIVEADDGUI' put: 16r10000!
ShellNotificationConstants at: 'SHCNE_DRIVEREMOVED' put: 16r80!
ShellNotificationConstants at: 'SHCNE_EXTENDED_EVENT' put: 16r4000000!
ShellNotificationConstants at: 'SHCNE_FREESPACE' put: 16r40000!
ShellNotificationConstants at: 'SHCNE_GLOBALEVENTS' put: 16rC0581E0!
ShellNotificationConstants at: 'SHCNE_INTERRUPT' put: 16r80000000!
ShellNotificationConstants at: 'SHCNE_MEDIAINSERTED' put: 16r20!
ShellNotificationConstants at: 'SHCNE_MEDIAREMOVED' put: 16r40!
ShellNotificationConstants at: 'SHCNE_MKDIR' put: 16r8!
ShellNotificationConstants at: 'SHCNE_NETSHARE' put: 16r200!
ShellNotificationConstants at: 'SHCNE_NETUNSHARE' put: 16r400!
ShellNotificationConstants at: 'SHCNE_RENAMEFOLDER' put: 16r20000!
ShellNotificationConstants at: 'SHCNE_RENAMEITEM' put: 16r1!
ShellNotificationConstants at: 'SHCNE_RMDIR' put: 16r10!
ShellNotificationConstants at: 'SHCNE_SERVERDISCONNECT' put: 16r4000!
ShellNotificationConstants at: 'SHCNE_UPDATEDIR' put: 16r1000!
ShellNotificationConstants at: 'SHCNE_UPDATEIMAGE' put: 16r8000!
ShellNotificationConstants at: 'SHCNE_UPDATEITEM' put: 16r2000!
ShellNotificationConstants shrink!

"Classes"!

NOTIFYREGISTER guid: (GUID fromString: '{004ABB62-5C68-4581-B61A-7E4CFF997500}')!
NOTIFYREGISTER comment: 'NOTIFYREGISTER is an <ExternalStructure> that wraps the WIndows NOTIFYREGISTER structure.  It''s used by a ShellNotification to register with WIndows that a shell folder should be monitored for changes.  #pidlPath specifies the folder to monitor and #bWatchSubtree directs whether all subFolders of the folder should be watched.'!
!NOTIFYREGISTER categoriesForClass!Unclassified! !
!NOTIFYREGISTER methodsFor!

bWatchSubtree
	"Answer the receiver's bWatchSubtree field as a Smalltalk object."

	^(bytes dwordAtOffset: 4) asBoolean!

bWatchSubtree: anObject
	"Set the receiver's bWatchSubtree field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject asParameter!

pidlPath
	"Answer the receiver's pidlPath field as a Smalltalk object."

	^ITEMIDLIST fromAddress: (bytes sdwordAtOffset: 0)!

pidlPath: anObject
	"Set the receiver's pidlPath field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject yourAddress! !
!NOTIFYREGISTER categoriesFor: #bWatchSubtree!**compiled accessors**!public! !
!NOTIFYREGISTER categoriesFor: #bWatchSubtree:!**compiled accessors**!public! !
!NOTIFYREGISTER categoriesFor: #pidlPath!**compiled accessors**!public! !
!NOTIFYREGISTER categoriesFor: #pidlPath:!**compiled accessors**!public! !

!NOTIFYREGISTER class methodsFor!

defineFields
	"
		NOTIFYREGISTER compileDefinition

	typedef struct {
		LPCITEMIDLIST pidlPath;
		BOOL bWatchSubtree;
	} NOTIFYREGISTER;"

	self
		defineField: #pidlPath type: (PointerField type: ITEMIDLIST);
		defineField: #bWatchSubtree type: BOOLField new! !
!NOTIFYREGISTER class categoriesFor: #defineFields!initializing!public! !

ShellNotification guid: (GUID fromString: '{4B5E8053-62E9-4DB9-A73E-9AE39206BBBD}')!
ShellNotification comment: 'ShellNotification is used with ShellNotificationView to trigger events when the Windows Shell changes.  When a ShellNotification is realized it registers itself with the singleton ShellNotificationView.  When a Shell change event occurs, Windows informs ShellNotificationView, which in turn triggers the event #shellChanged:eventId: in the ShellNotification.  Users of ShellNotification should install an observer on this event.

Instance Variables:
	parseName	<String> of a form that IShellFolder>>idlFromPath: can parse
	eventId		<Integer> one or more ShellNotificationConstants flags
	isWatchSubtree	<Boolean> whether subFolders should be watched'!
!ShellNotification categoriesForClass!Unclassified! !
!ShellNotification methodsFor!

assertNotRealized
	handle notNil ifTrue: [self error: 'The Notification is already registered']!

basicFree
	"Private - Free the handle.  Ignore any error return (e.g. due to an invalid handle)."

	ShellLibrary default SHChangeNotifyDeregister: handle!

createHandle
	"Private - Create and answer an external resource handle associated with the receiver.
	We ask the ShellNotificationView default instance for a WM_USER+userId message number
	and also register the receiver with the view.  The view keeps a weak table mapping  
	WM_USER+userId number to registered ShellNotification instances, and uses the table to 
	locate the appropriate ShellNotification instance for a windows message"

	^ShellLibrary default 
		SHChangeNotifyRegister: ShellNotificationView default asParameter
		dwFlags: 3
		wEventMask: self eventId
		uMsg: (ShellNotificationView default createUMsgFor: self)
		cItems: 1
		lpItems: ((NOTIFYREGISTER new)
				bWatchSubtree: self isWatchSubtree;
				pidlPath: self id;
				yourself)!

displayName
	^self shellObject displayName!

displayOn: aStream
	"Append to aStream a String representation of the receiver that a user would want to see."

	super printOn: aStream.
	aStream nextPut: $(.
	self displayName displayOn: aStream.
	aStream nextPut: $)!

eventId
	eventId isNil ifTrue: [eventId := SHCNE_DISKEVENTS].
	^eventId!

eventId: aSHCNEConstant
	eventId := aSHCNEConstant!

id
	^self shellObject idFull!

isPersistent
	"Private - Answer true if the receiver holds sufficient information such that
	it can recreate itself when the image is saved and restored. "

	^true!

isWatching
	^self isRealized!

isWatchSubtree
	isWatchSubtree isNil ifTrue: [isWatchSubtree := false].
	^isWatchSubtree!

isWatchSubtree: aBoolean
	isWatchSubtree := aBoolean!

parseName
	^parseName!

parseName: aString
	parseName := aString!

shellObject
	"Answer the ShellObject that the receiver represents."

	^ShellFolder fromParseName: self parseName!

uMsg
	"Answer the WM_USER+userId message that the receiver has registered, or 0 if not registered"

	^ShellNotificationView default uMsgFor: self ifAbsent: [0]!

watch
	self assertNotRealized.
	self realize!

watchFiles
	self assertNotRealized.
	eventId := self class diskEventId.
	isWatchSubtree := false.
	self realize!

watchFolder
	self assertNotRealized.
	eventId := self class folderEventId.
	isWatchSubtree := false.
	self realize!

watchFolders
	self assertNotRealized.
	eventId := self class folderEventId.
	isWatchSubtree := true.
	self realize! !
!ShellNotification categoriesFor: #assertNotRealized!public!realizing/unrealizing! !
!ShellNotification categoriesFor: #basicFree!private!realizing/unrealizing! !
!ShellNotification categoriesFor: #createHandle!private!realizing/unrealizing! !
!ShellNotification categoriesFor: #displayName!accessing!public! !
!ShellNotification categoriesFor: #displayOn:!displaying!public! !
!ShellNotification categoriesFor: #eventId!accessing!private! !
!ShellNotification categoriesFor: #eventId:!accessing!public! !
!ShellNotification categoriesFor: #id!accessing!public! !
!ShellNotification categoriesFor: #isPersistent!private!testing! !
!ShellNotification categoriesFor: #isWatching!public!testing! !
!ShellNotification categoriesFor: #isWatchSubtree!public!testing! !
!ShellNotification categoriesFor: #isWatchSubtree:!accessing!public! !
!ShellNotification categoriesFor: #parseName!accessing!public! !
!ShellNotification categoriesFor: #parseName:!accessing!public! !
!ShellNotification categoriesFor: #shellObject!accessing!public! !
!ShellNotification categoriesFor: #uMsg!accessing!public! !
!ShellNotification categoriesFor: #watch!public!realizing/unrealizing! !
!ShellNotification categoriesFor: #watchFiles!public!realizing/unrealizing! !
!ShellNotification categoriesFor: #watchFolder!public!realizing/unrealizing! !
!ShellNotification categoriesFor: #watchFolders!public!realizing/unrealizing! !

!ShellNotification class methodsFor!

diskEventId
	^SHCNE_DISKEVENTS!

folderEventId
	^SHCNE_DRIVEADD | SHCNE_DRIVEREMOVED | SHCNE_MEDIAINSERTED | SHCNE_MEDIAREMOVED 
		| SHCNE_MKDIR | SHCNE_NETSHARE 
		| SHCNE_NETUNSHARE | SHCNE_RENAMEFOLDER 
		| SHCNE_RMDIR!

forParseName: aString
	"Answer an instance of the receiver that monitors changes to the ShellObject represented by aString."

	^(self new)
		parseName: aString;
		yourself!

forShellObject: aShellObject
	"Answer an instance of the receiver that monitors changes to aShellObject."

	^self forParseName: aShellObject parseName!

publishedEventsOfInstances
	"Answer a Set of Symbols that describe the published events triggered
	by instances of the receiver."

	^(super publishedEventsOfInstances)
		add: #shellChanged:eventId:;
		yourself! !
!ShellNotification class categoriesFor: #diskEventId!constants!public! !
!ShellNotification class categoriesFor: #folderEventId!constants!public! !
!ShellNotification class categoriesFor: #forParseName:!instance creation!public! !
!ShellNotification class categoriesFor: #forShellObject:!instance creation!public! !
!ShellNotification class categoriesFor: #publishedEventsOfInstances!development!public! !

ShellNotificationViewer guid: (GUID fromString: '{6F0A49D3-FC30-4970-8A7D-1DA165B52945}')!
ShellNotificationViewer comment: 'ShellNotificationViewer demonstrates how an application can register ShellNotifications with Windows, allowing the application to be notified of any specified changes that were made in any specified part of the Shell namespace, from the desktop folder on down.

	ShellNotificationViewer show'!
!ShellNotificationViewer categoriesForClass!Unclassified! !
!ShellNotificationViewer methodsFor!

addFilesNotification
	| path |
	(path := BrowseFolderDialog showModal) isNil ifTrue: [^nil].
	self addNotification: (ShellNotification forParseName: path) watchFiles!

addFoldersNotification
	self 
		addNotification: (ShellNotification forShellObject: ShellFolder desktop) watchFolders!

addNotification
	| path isWatchSubTree eventID eventIDs |
	(path := BrowseFolderDialog showModal) isNil ifTrue: [^nil].
	isWatchSubTree := MessageBox confirm: 'Watch SubFolders?'.
	(eventIDs := ChoicePrompter 
				on: (OrderedCollection with: 'SHCNE_DISKEVENTS') asValue
				multipleChoices: ShellNotificationConstants keys asSortedCollection
				caption: 'Enter the Event ID masks') isNil 
		ifTrue: [^nil].
	eventID := 0.
	eventIDs do: [:each | eventID := eventID | (ShellNotificationConstants at: each)].
	self addNotification: ((ShellNotification forParseName: path)
				isWatchSubtree: isWatchSubTree;
				eventId: eventID;
				watch)!

addNotification: aShellNotification
	"Install an observer on aShellNotification and add it the receiver's list of notifications."

	aShellNotification 
		when: #shellChanged:eventId:
		send: #onShellChanged:eventId:
		to: self.
	notificationsListPresenter model add: aShellNotification!

clearEventList
	eventListPresenter model removeAll!

createComponents
	super createComponents.
	notificationsListPresenter := self add: ListPresenter new name: 'notificationsList'.
	eventListPresenter := self add: ListPresenter new name: 'eventList'!

deleteNotification
	"Remove the receiver's selected ShellNofication from its list of notifications.
	Note that removing the reference to the ShellNotification is sufficient to remove it from the system 
	since it is held by a WeakIdentityDictionary (in ShellNofiticationView)."

	| selection |
	(selection := notificationsListPresenter selectionOrNil) isNil ifTrue: [^nil].
	notificationsListPresenter model remove: selection!

flagNamesFor: anInteger
	^ShellNotificationConstants keys asOrderedCollection 
		select: [:each | anInteger anyMask: (ShellNotificationConstants at: each)]!

helpAbout
	(MessageBox new)
		caption: 'About ' , self class name asString;
		icon: self class icon;
		text: self class aboutText;
		open!

initialize
	super initialize.
	isCapture := true!

onShellChanged: aShellNotification eventId: anEventID
	| array |
	isCapture ifFalse: [^nil].
	array := Array new: 4.
	array
		at: 1 put: aShellNotification uMsg;
		at: 2 put: aShellNotification displayName;
		at: 3 put: anEventID;
		at: 4 put: (self flagNamesFor: anEventID) asArray.
	eventListPresenter model add: array!

onViewClosed
	"Sent by the receiver's view when it has been closed.
	Disconnect from any events triggered "

	super onViewClosed.
	notificationsListPresenter model 
		ifNotNil: [:list | list do: [:each | each removeEventsTriggeredFor: self]]!

queryCommand: query
	"Private - Enters details about a potential command for the receiver into the 
	<CommandQuery>,  query."

	| command |
	command := query command.
	#deleteNotification == command 
		ifTrue: 
			[query isEnabled: notificationsListPresenter hasSelection.
			^true].
	#toggleWatching == command 
		ifTrue: 
			[| hasSelection |
			hasSelection := notificationsListPresenter hasSelection.
			query
				isEnabled: hasSelection;
				isChecked: (hasSelection and: [notificationsListPresenter selection isWatching]).
			^true].
	#toggleCapture == command 
		ifTrue: 
			[query
				isEnabled: true;
				isChecked: isCapture.
			^true].
	^super queryCommand: query!

toggleCapture
	isCapture := isCapture not!

toggleWatching
	| selection |
	(selection := notificationsListPresenter selectionOrNil) isNil ifTrue: [^nil].
	selection isWatching ifTrue: [selection free] ifFalse: [selection watch].
	notificationsListPresenter view refreshContents.
	notificationsListPresenter selection: selection! !
!ShellNotificationViewer categoriesFor: #addFilesNotification!commands!public! !
!ShellNotificationViewer categoriesFor: #addFoldersNotification!commands!public! !
!ShellNotificationViewer categoriesFor: #addNotification!commands!public! !
!ShellNotificationViewer categoriesFor: #addNotification:!accessing!private! !
!ShellNotificationViewer categoriesFor: #clearEventList!commands!public! !
!ShellNotificationViewer categoriesFor: #createComponents!initializing!private! !
!ShellNotificationViewer categoriesFor: #deleteNotification!commands!public! !
!ShellNotificationViewer categoriesFor: #flagNamesFor:!helpers!private! !
!ShellNotificationViewer categoriesFor: #helpAbout!commands!public! !
!ShellNotificationViewer categoriesFor: #initialize!initializing!private! !
!ShellNotificationViewer categoriesFor: #onShellChanged:eventId:!event handling!public! !
!ShellNotificationViewer categoriesFor: #onViewClosed!event handling!public! !
!ShellNotificationViewer categoriesFor: #queryCommand:!commands!private! !
!ShellNotificationViewer categoriesFor: #toggleCapture!commands!public! !
!ShellNotificationViewer categoriesFor: #toggleWatching!commands!public! !

!ShellNotificationViewer class methodsFor!

aboutText
	^
'Shell Notification Viewer

Copyright: 2002 
  Steve Waring <swaring@ozemail.com.au>
  Louis Sumberg <lsumberg@mindspring.com>

Version: %1 

DISCLAIMER: This software is freely provided purely as a sample and as such it
is provided "as is", WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE' 
		formatWith: SessionManager current versionInfo productVersionString
!

icon
	^Icon fromId: 174 in: ShellLibrary default
! !
!ShellNotificationViewer class categoriesFor: #aboutText!enquiries!public! !
!ShellNotificationViewer class categoriesFor: #icon!constants!public! !

ShellNotificationView guid: (GUID fromString: '{6CBEE1A7-2197-48A7-8BED-434B514CEB64}')!
ShellNotificationView comment: 'The singleton instance of ShellNotificationView is used to store registered ShellNotifications and to route subsequent shell notification events (user messages) from Windows to the appropriate ShellNotification(s).

Instance Variables
	notifications <WeakIdentityDictionary> stores registered ShellNotifications keyed by their user message id'!
!ShellNotificationView categoriesForClass!Unclassified! !
!ShellNotificationView methodsFor!

allNotifications
	"Answer a collection of the registered <ShellNotification>s"

	^notifications values!

atNextEmpty: uMsg put: aShellNotification
	"Put the shellNotification at the next available uMsg number"

	^(notifications at: uMsg ifAbsentPut: [aShellNotification]) == aShellNotification 
		ifTrue: [uMsg]
		ifFalse: [self atNextEmpty: uMsg + 1 put: aShellNotification]!

createUMsgFor: aShellNotification
	"Create (or reuse) a User Message number for the shell notification.
	A win user message with this number will cause the #shellChanged:eventId: event to be triggered off aShellNotification"

	^notifications keyAtValue: aShellNotification
		ifAbsent: [self newUMsgFor: aShellNotification]!

dispatchUser: userId wParam: wParam lParam: lParam
	"Private - Dispatch the WM_USER+userId message which was sent to the receiver."

	self 
		wmShellChangeNotification: userId
		wParam: wParam
		lParam: lParam!

initialize
	super initialize.
	notifications := WeakIdentityDictionary new: 10!

minimize
	!

newUMsgFor: aShellNotification
	"Answer the uMsg number that the receiver places the shellNotification into its notifications table.
	The shellNotification uses this number when it registers itself (see ShellNotification>>createHandle)"

	^self atNextEmpty: WM_USER + self class defaultUserOffset put: aShellNotification!

notifications
	^notifications!

notifications: anObject
	notifications := anObject!

notifyAbsentUMsg: userId
	^Notification 
		signal: 'Received ShellNotification for absent uMsg: ' , userId displayString!

uMsgFor: aShellNotification ifAbsent: operation
	"Answer the WM_USER+userId message that the receiver has registered for aShellNotification, 
	else the value of operation if aShellNotification is not registered."

	^notifications keyAtValue: aShellNotification ifAbsent: operation!

wmShellChangeNotification: userId wParam: wParam lParam: lParam
	"Locate the <ShellNotification> that is mapped to userId and trigger the #shellChanged:eventId:
	event from it.  Use a deferredAction so that we quickly process multiple events"

	| notification |
	notification := notifications at: WM_USER + userId
				ifAbsent: [self notifyAbsentUMsg: WM_USER + userId].
	SessionManager inputState queueDeferredAction: 
			[notification 
				trigger: #shellChanged:eventId:
				with: notification
				with: lParam]! !
!ShellNotificationView categoriesFor: #allNotifications!accessing!public! !
!ShellNotificationView categoriesFor: #atNextEmpty:put:!accessing!public! !
!ShellNotificationView categoriesFor: #createUMsgFor:!accessing!public! !
!ShellNotificationView categoriesFor: #dispatchUser:wParam:lParam:!dispatching!private! !
!ShellNotificationView categoriesFor: #initialize!initializing!private! !
!ShellNotificationView categoriesFor: #minimize!operations!public! !
!ShellNotificationView categoriesFor: #newUMsgFor:!accessing!public! !
!ShellNotificationView categoriesFor: #notifications!accessing!private! !
!ShellNotificationView categoriesFor: #notifications:!accessing!private! !
!ShellNotificationView categoriesFor: #notifyAbsentUMsg:!exceptions!private! !
!ShellNotificationView categoriesFor: #uMsgFor:ifAbsent:!accessing!public! !
!ShellNotificationView categoriesFor: #wmShellChangeNotification:wParam:lParam:!event handling-win32!private! !

!ShellNotificationView class methodsFor!

default
	"Answer the singleton instance of the receiver."

	DefaultInstance isNil 
		ifTrue: 
			[DefaultInstance := (self basicNew initialize)
						parentView: View desktop;
						create;
						yourself].
	^DefaultInstance!

defaultUserOffset
	^280!

new
	"The receiver is a singleton, use #default."

	^self shouldNotImplement!

onStartup
	"Re-register all <ShellNotification>s that were saved with the image."

	DefaultInstance notNil 
		ifTrue: 
			[| oldNotifications |
			oldNotifications := DefaultInstance allNotifications.
			DefaultInstance := nil.
			SessionManager inputState 
				queueDeferredAction: [oldNotifications do: [:each | each watch]]]!

uninitialize
	"Private - Uninitialize the receiver as it is about to be removed from the system."

	DefaultInstance notNil ifTrue: [
		DefaultInstance destroy.
		DefaultInstance := nil].! !
!ShellNotificationView class categoriesFor: #default!accessing!public! !
!ShellNotificationView class categoriesFor: #defaultUserOffset!constants!public! !
!ShellNotificationView class categoriesFor: #new!instance creation!public! !
!ShellNotificationView class categoriesFor: #onStartup!event handling!public! !
!ShellNotificationView class categoriesFor: #uninitialize!initializing!private! !

"Binary Globals"!

"Resources"!

(ResourceIdentifier class: ShellNotificationViewer name: 'Default view') assign: (Object fromBinaryStoreBytes:
(ByteArray fromBase64String: 'IVNUQiAxIEYCDAABAAAAVmlld1Jlc291cmNlAAAAAA4BJABTVEJSZXNvdXJjZVNUQkJ5dGVBcnJh
eUFjY2Vzc29yUHJveHkAAAAAcgAAAOEVAAAhU1RCIDEgTggMAAoAAABTVEJWaWV3UHJveHkAAAAA
mgAAAAAAAABSAAAAEAAAAERvbHBoaW4gTVZQIEJhc2VSAAAACQAAAFNoZWxsVmlld2IAAAAbAAAA
AAAAAAAAAABiAAAAAgAAAAEAngEBAAIAoAEAAAAAAAAAAAAAAAAAAAcCAAAAAAAAAAAAAAAAAACg
AQAABgISAFByb3BvcnRpb25hbExheW91dAAAAADqAAAAAAAAAPAAAABiAAAAAAAAABAAAADqAAAA
AAAAAAABAABiAAAABAAAAJoBAAAAAAAAmgAAAAAAAABSAAAAFwAAAERvbHBoaW4gQ29tbW9uIENv
bnRyb2xzUgAAAAgAAABMaXN0Vmlld2IAAAAeAAAAAAAAAKABAABiAAAAAgAAAIIAAAAEAAAATZAB
RAEEAABgAgAARgMJAAIAAABMaXN0TW9kZWwAAAAAygAAAAAAAADQAAAAMAIAAAAAAAAGABQASWRl
bnRpdHlTZWFyY2hQb2xpY3kAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAYAIAAAAAAACCAAAA
CAAAAKUH//8AAAAAmgAAAAAAAADAAQAAUgAAABEAAABCYXNpY0xpc3RBYnN0cmFjdAAAAAAOAhEA
U1RCU2luZ2xldG9uUHJveHkAAAAAmgAAAAAAAADAAQAAUgAAABAAAABJY29uSW1hZ2VNYW5hZ2Vy
ugAAAAAAAABSAAAABwAAAGN1cnJlbnQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADKAAAAAAAAANAA
AABiAAAABAAAAEYMDgAFAAAATGlzdFZpZXdDb2x1bW4AAAAAUgAAAAQAAABVTXNnZQAAALoAAAAA
AAAAUgAAAAQAAABsZWZ0BgQMAEJsb2NrQ2xvc3VyZQAAAAAmAw0ATWV0aG9kQ29udGV4dAEAAAAB
AAAAJgUSAENvbXBpbGVkRXhwcmVzc2lvbgEAAACBAQAAmgAAAAAAAABSAAAABwAAAERvbHBoaW5S
AAAADwAAAFVuZGVmaW5lZE9iamVjdFIAAAAEAAAAZG9JdGIAAAACAAAAUgAAABQAAABbOml0ZW0g
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
ZGVmYXVsdA=='))!

