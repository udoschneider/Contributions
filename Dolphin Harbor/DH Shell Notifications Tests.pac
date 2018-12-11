| package |
package := Package name: 'DH Shell Notifications Tests'.
package paxVersion: 0;
	basicComment: ''.

package basicPackageVersion: '5.1.2.1'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAxIAAAAAA=').

package classNames
	add: #ShellNotificationTest;
	add: #ShellNotificationViewTest;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package allResourceNames: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'DH Shell Core';
	add: 'DH Shell Notifications';
	add: 'DH Shell Tests';
	add: '..\..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Camp Smalltalk\SUnit\SUnit';
	yourself).

package!

"Class Definitions"!

TestCase subclass: #ShellNotificationTest
	instanceVariableNames: 'shellNotification'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TestCase subclass: #ShellNotificationViewTest
	instanceVariableNames: 'view'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

ShellNotificationTest guid: (GUID fromString: '{D51E1E59-CC16-4D86-82AB-4E4E0F39A51B}')!
ShellNotificationTest comment: ''!
!ShellNotificationTest categoriesForClass!Unclassified! !
!ShellNotificationTest methodsFor!

setForEmptyFolder
	| shellFolder |
	shellFolder := ShellFolder fromPath: ShellObjectResource current pathOfEmptyFolder.
	shellNotification := ShellNotification forShellObject: shellFolder.
!

testCreationFromDesktop
	| shellFolder |
	shellNotification := ShellNotification forShellObject: ShellFolder desktop.
	self assert: shellNotification shellObject = ShellFolder desktop.
	self assert: shellNotification shellObject isDesktop!

testCreationFromShellObject
	| shellFolder |
	shellFolder := ShellFolder fromPath: ShellObjectResource current pathOfEmptyFolder.
	shellNotification := ShellNotification forShellObject: shellFolder.
	self assert: shellNotification shellObject = shellFolder.
	self assert: shellNotification parseName = shellFolder parseName!

testIsWatching
	self setForEmptyFolder.
	self deny: shellNotification isWatching.
	shellNotification watch.
	self assert: shellNotification isWatching.
	shellNotification free.
	self deny: shellNotification isWatching!

testShellObject
	self setForEmptyFolder.
	self assert: (shellNotification shellObject isKindOf: ShellObject)!

testWatchFiles
	| id |
	self setForEmptyFolder.
	shellNotification watchFiles.
	self deny: shellNotification isWatchSubtree.
	self assert: shellNotification isRealized.
	self 
		assert: (ShellNotificationView default notifications values includes: shellNotification).
	self should: [shellNotification watchFiles] raise: Error.
	shellNotification free! !
!ShellNotificationTest categoriesFor: #setForEmptyFolder!accessing!public! !
!ShellNotificationTest categoriesFor: #testCreationFromDesktop!public!tests! !
!ShellNotificationTest categoriesFor: #testCreationFromShellObject!public!tests! !
!ShellNotificationTest categoriesFor: #testIsWatching!public!tests! !
!ShellNotificationTest categoriesFor: #testShellObject!public!tests! !
!ShellNotificationTest categoriesFor: #testWatchFiles!public!tests! !

!ShellNotificationTest class methodsFor!

resources
	^Array with: ShellObjectResource! !
!ShellNotificationTest class categoriesFor: #resources!accessing!public! !

ShellNotificationViewTest guid: (GUID fromString: '{61B376C5-8B0B-4B50-A06B-4748342A98E0}')!
ShellNotificationViewTest comment: ''!
!ShellNotificationViewTest categoriesForClass!Unclassified! !
!ShellNotificationViewTest methodsFor!

setUp
	super setUp.
	view := ShellNotificationView basicNew initialize!

testMultipleUMsg
	| o1 o2 m1 m2 |
	o1 := Object new.
	o2 := Object new.
	m1 := view createUMsgFor: o1.
	m2 := view createUMsgFor: o2.
	self deny: m1 = m2.
	self assert: m1 = (view createUMsgFor: o1).
	self assert: m2 = (view createUMsgFor: o2).
	self assert: (view notifications at: m1) == o1.
	self assert: (view notifications at: m2) == o2!

testUMsgReuse
	| o1 o2 o3 m1 m2 m3 |
	o1 := Object new.
	o2 := Object new.
	m1 := view createUMsgFor: o1.
	m2 := view createUMsgFor: o2.
	view notifications removeKey: m1.
	self assert: (m3 := view createUMsgFor: (o3 := Object new)) < m2! !
!ShellNotificationViewTest categoriesFor: #setUp!public!setup! !
!ShellNotificationViewTest categoriesFor: #testMultipleUMsg!public!tests! !
!ShellNotificationViewTest categoriesFor: #testUMsgReuse!public!tests! !

"Binary Globals"!

"Resources"!

