;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2025 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog.lisp                                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The CLOG package and its exports.  Upstream defines them with MGL-PAX's
;;; DEFSECTIONs, which also make CLOG's manual; this fork lists the same
;;; symbols, grouped by those sections, less those of the parts it leaves out
;;; (CLOG-DBI, the SQL helpers of CLOG-DATA, the tutorial and builder helpers,
;;; the jQuery objects of clog-jquery.lisp) and the sections' own names.  The
;;; physical files map in most cases to the sections, except CLOG-Obj, which
;;; is defined in clog-base.lisp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports - clog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog
  (:documentation "CLOG - The Common List Omnificent GUI")
  (:import-from #:clog-connection
                #:*clog-port*
                #:*static-root*
                #:make-hash-table*
                #:escape-string
                #:generate-id
                #:random-hex-string)
  (:use #:cl #:parse-float)
  (:export
   ;;; CLOG System
   ;; CLOG Startup and Shutdown
   #:initialize
   #:*static-root*
   #:*clog-port*
   #:*clog-debug*
   #:set-on-new-window
   #:is-running-p
   #:shutdown
   #:debug-mode
   #:open-file-with-os
   #:open-browser
   ;;; CLOG Utilities
   ;; Concurrent Hash Tables
   #:make-hash-table*
   ;; Declerative Syntax Support
   #:with-clog-create
   ;; CLOG ID utilities
   #:generate-id
   #:random-hex-string
   ;; CLOG JS utilities
   #:js-true-p
   #:p-true-js
   #:js-on-p
   #:p-on-js
   #:escape-for-html
   #:escape-string
   #:lf-to-br
   #:js-to-integer
   #:js-to-float
   ;; CLOG Color utilities
   #:rgb
   #:rgb-to-hex
   #:rgba
   #:hsl
   #:hsla
   ;; CLOG Unit utilities
   #:unit
   #:unit*
   ;;; CLOG Objects
   ;; CLOG-Obj - Base class for CLOG Objects
   #:clog-obj
   #:parent
   ;; CLOG-Obj - General Properties
   #:property
   ;; CLOG-Obj - General Methods
   #:height
   #:width
   #:focus
   #:scroll-into-view
   #:blur
   ;; CLOG-Obj - Low Level
   #:*store-new-objects*
   #:connection-data
   #:connection-data-item
   #:remove-connection-data-item
   #:connection-body
   #:connection-path
   #:connection-sync
   #:with-sync-event
   #:validp
   #:with-connection-cache
   #:flush-connection-cache
   ;; CLOG-Obj - Internals for Extensions and Plugins
   #:html-id
   #:script-id
   #:execute
   #:query
   #:js-execute
   #:js-query
   #:set-on-event
   #:set-on-event-with-data
   ;; CLOG-Obj - Event Handling
   #:set-on-resize
   #:set-on-focus
   #:set-on-blur
   #:set-on-change
   #:set-on-input
   #:set-on-drag-start
   #:set-on-drag
   #:set-on-drag-end
   #:set-on-drag-enter
   #:set-on-drag-leave
   #:set-on-drag-over
   #:set-on-drop
   #:set-on-focus-in
   #:set-on-focus-out
   #:set-on-reset
   #:set-on-search
   #:set-on-select
   #:set-on-submit
   #:set-on-context-menu
   #:set-on-click
   #:set-on-double-click
   #:set-on-mouse-click
   #:set-on-mouse-double-click
   #:set-on-mouse-right-click
   #:set-on-mouse-enter
   #:set-on-mouse-leave
   #:set-on-mouse-over
   #:set-on-mouse-out
   #:set-on-mouse-down
   #:set-on-mouse-up
   #:set-on-mouse-move
   #:set-on-wheel
   #:set-on-pointer-enter
   #:set-on-pointer-leave
   #:set-on-pointer-over
   #:set-on-pointer-out
   #:set-on-pointer-down
   #:set-on-pointer-up
   #:set-on-pointer-cancel
   #:set-on-pointer-move
   #:set-on-touch-start
   #:set-on-touch-move
   #:set-on-touch-end
   #:set-on-touch-cancel
   #:set-on-character
   #:set-on-key-down
   #:set-on-key-up
   #:set-on-key-press
   #:set-on-copy
   #:set-on-cut
   #:set-on-paste
   ;;; CLOG Elements
   ;; CLOG-Element - Class for CLOG Elements
   #:clog-element
   ;; CLOG-Element - Low Level
   #:create-element
   #:create-child
   #:attach-as-child
   #:destroy-children
   ;; CLOG-Element - DOM Placement
   #:place-after
   #:place-before
   #:place-inside-top-of
   #:place-inside-bottom-of
   #:place-text-inside-top-of
   #:place-text-inside-bottom-of
   ;; CLOG-Element - General Properties
   #:style
   #:set-styles
   #:attribute
   #:remove-attribute
   #:has-attribute
   ;; CLOG-Element - Properties
   #:access-key
   #:advisory-title
   #:css-class-name
   #:editablep
   #:draggablep
   #:visiblep
   #:hiddenp
   #:inner-html
   #:outer-html
   #:spellcheckp
   #:tab-index
   #:text
   #:text-value
   #:text-direction-type
   #:text-direction
   #:language-code
   #:position-left
   #:position-top
   #:client-left
   #:client-top
   #:client-width
   #:client-height
   #:offset-left
   #:offset-top
   #:offset-width
   #:offset-height
   #:scroll-left
   #:scroll-top
   #:scroll-width
   #:scroll-height
   #:html-tag
   ;; CLOG-Element - Styles
   #:box-sizing-type
   #:box-sizing
   #:clear-side-type
   #:clear-side
   #:float-wrap-type
   #:float-wrap
   #:display-type
   #:display
   #:order
   #:flex
   #:set-flex
   #:flex-wrap-type
   #:flex-wrap
   #:flex-direction-type
   #:flex-direction
   #:grid-template
   #:grid-template-columns
   #:grid-template-rows
   #:grid-template-areas
   #:column-gap
   #:row-gap
   #:grid-auto-columns
   #:grid-auto-rows
   #:grid-auto-flow
   #:grid-column
   #:grid-column-start
   #:grid-column-end
   #:grid-row
   #:grid-row-start
   #:grid-row-end
   #:grid-area
   #:align-items-type
   #:align-items
   #:align-self-type
   #:align-self
   #:align-content-type
   #:align-content
   #:justify-items-type
   #:justify-items
   #:justify-self-type
   #:justify-self
   #:justify-content-type
   #:justify-content
   #:overflow-type
   #:overflow
   #:overflow-x-type
   #:overflow-x
   #:overflow-y-type
   #:overflow-y
   #:z-index
   #:resizable-type
   #:resizable
   #:positioning-type
   #:positioning
   #:set-geometry
   #:left
   #:top
   #:right
   #:bottom
   #:box-height
   #:box-width
   #:maximum-height
   #:maximum-width
   #:minimum-height
   #:minimum-width
   #:inner-height
   #:inner-width
   #:outer-height
   #:outer-width
   #:outer-height-to-margin
   #:outer-width-to-margin
   #:color
   #:opacity
   #:background-attachment-type
   #:background-attachment
   #:background-color
   #:background-image
   #:background-position
   #:background-origin-type
   #:background-origin
   #:background-repeat-type
   #:background-repeat
   #:background-clip-type
   #:background-clip
   #:background-size
   #:border-style-type
   #:border
   #:set-border
   #:border-radius
   #:box-shadow
   #:text-shadow
   #:outline-style-type
   #:outline
   #:set-outline
   #:margin
   #:set-margin
   #:set-margin-side
   #:padding
   #:set-padding
   #:set-padding-side
   #:cursor
   #:font-style-type
   #:font-variant-type
   #:system-font-type
   #:font
   #:font-css
   #:set-font
   #:text-alignment-type
   #:text-alignment
   #:vertical-align-type
   #:vertical-align
   ;; CLOG-Element - Methods
   #:add-class
   #:remove-class
   #:toggle-class
   #:remove-from-dom
   #:remove-from-clog
   #:destroy
   #:browser-gc
   #:click
   #:replace-element
   #:replace-children
   #:swap-element-by-id
   ;; CLOG-Element - DOM Traversal Methods
   #:parent-element
   #:first-child
   #:previous-sibling
   #:next-sibling
   #:list-of-children
   ;;; Common CLOG Elements
   ;; CLOG-A - Class for CLOG Anchors
   #:clog-a
   #:create-a
   #:link
   #:target
   #:download
   ;; CLOG-BR - Class for CLOG Line Breaks
   #:clog-br
   #:create-br
   ;; CLOG-BUTTON - Class for CLOG Buttons
   #:clog-button
   #:create-button
   #:disabledp
   ;; CLOG-IMG - Class for CLOG Images
   #:clog-img
   #:create-img
   #:url-src
   ;; CLOG-Div - Class for CLOG Div Blocks
   #:clog-div
   #:create-div
   ;; CLOG-Dialog - Class for CLOG Dialog Blocks
   #:clog-dialog
   #:create-dialog
   #:return-value
   #:dialog-openp
   #:show-dialog
   #:close-dialog
   ;; CLOG-Details - Class for CLOG Detail Blocks
   #:clog-details
   #:create-details
   #:details-openp
   ;; CLOG-Summary - Class for CLOG Summary Blocks
   #:clog-summary
   #:create-summary
   ;; CLOG-HR - Class for CLOG Hortizontal Rules
   #:clog-hr
   #:create-hr
   ;; CLOG-Meter - Class for CLOG Meters
   #:clog-meter
   #:create-meter
   #:value
   #:high
   #:low
   #:minimum
   #:maximum
   #:optimum
   ;; CLOG-Progress-Bar - Class for CLOG Progress Bars
   #:clog-progress-bar
   #:create-progress-bar
   ;; CLOG-P - Class for CLOG Paragraphs
   #:clog-p
   #:create-p
   ;; CLOG-Span - Class for CLOG Inline Spans
   #:clog-span
   #:create-span
   ;; CLOG-Section - Class for CLOG Inline Sections
   #:section-type
   #:clog-section
   #:create-section
   ;; CLOG-Phrase - Class for CLOG Inline Phrases
   #:phrase-type
   #:clog-phrase
   #:create-phrase
   ;; CLOG-Ordered-List - Class for CLOG Ordered-Lists
   #:clog-ordered-list
   #:create-ordered-list
   #:list-kind-type
   #:list-kind
   #:list-location-type
   #:list-location
   ;; CLOG-Unordered-List - Class for CLOG Unordered-Lists
   #:clog-unordered-list
   #:create-unordered-list
   ;; CLOG-List-Item - Class for CLOG List-Items
   #:clog-list-item
   #:create-list-item
   #:item-value
   ;; CLOG-Definition-List - Class for CLOG Definition-Lists
   #:clog-definition-list
   #:create-definition-list
   ;; CLOG-Term - Class for CLOG Terms
   #:clog-term
   #:create-term
   ;; CLOG-Description - Class for CLOG Descriptions
   #:clog-description
   #:create-description
   ;; CLOG-Table - Class for CLOG Tables
   #:clog-table
   #:create-table
   ;; CLOG-Table-Row - Class for CLOG Table-Rows
   #:clog-table-row
   #:create-table-row
   ;; CLOG-Table-Column - Class for CLOG Table-Columns
   #:clog-table-column
   #:create-table-column
   ;; CLOG-Table-Heading - Class for CLOG Table-Headings
   #:clog-table-heading
   #:create-table-heading
   ;; CLOG-Table-Head - Class for CLOG Table-Heads
   #:clog-table-head
   #:create-table-head
   ;; CLOG-Table-Body - Class for CLOG Table-Bodys
   #:clog-table-body
   #:create-table-body
   ;; CLOG-Table-Caption - Class for CLOG Table-Captions
   #:clog-table-caption
   #:create-table-caption
   ;; CLOG-Table-Footer - Class for CLOG Table-Footers
   #:clog-table-footer
   #:create-table-footer
   ;; CLOG-Table-Column-Group - Class for CLOG Table-Column-Groups
   #:clog-table-column-group
   #:create-table-column-group
   #:clog-table-column-group-item
   #:create-table-column-group-item
   ;;; CLOG Presentations
   ;; CLOG-Presentations - CLOG bindings to Lisp Objects
   #:link-slot-and-form-element
   #:link-slot-and-element
   #:link-form-element-to-slot
   #:link-element-to-slot
   #:link-element-to-place
   #:link-slot-to-form-element
   #:link-slot-to-element
   #:link-slot-to-place
   ;;; CLOG Data
   ;; Load and Write to objects and CLOG-Elements
   #:data-load-plist
   #:data-write-list
   #:data-write-plist
   ;;; CLOG Panels
   ;; CLOG-Panel - CLOG Panels
   #:clog-panel
   #:create-panel
   ;; CLOG-Panel-Box - CLOG Panel Box
   #:clog-panel-box
   #:create-panel-box
   #:panel-box
   ;; CLOG-Panel-Box-Layout
   #:clog-panel-box-layout
   #:envelope-panel
   #:envelope-panel*
   #:center-children
   #:create-panel-box-layout
   #:center-panel
   #:top-panel
   #:left-panel
   #:right-panel
   #:bottom-panel
   #:fit-layout
   ;;; CLOG Tree
   ;; CLOG-Tree - CLOG Trees
   #:clog-tree
   #:create-clog-tree
   #:tree-root
   #:toggle-tree
   #:toggle-state
   #:indent-level
   #:content
   #:clog-tree-item
   #:create-clog-tree-item
   #:tree-item
   ;;; CLOG Style Blocks
   ;; CLOG-Style-Block - CLOG Style Blocks
   #:clog-style-block
   #:create-style-block
   #:add-style
   ;;; CLOG Form Objects
   ;; CLOG-Form-Data
   #:form-get-data
   #:form-post-data
   #:form-multipart-data
   #:delete-multipart-data
   #:form-data-item
   ;; CLOG-Form - Class for organizing Form Elements in to a From
   #:form-method-type
   #:clog-form
   #:create-form
   #:form-element-count
   #:submit
   #:reset
   #:autocompletep
   #:encoding
   #:validate-on-submit
   ;; CLOG-Fieldset - Class for CLOG Fieldsets
   #:clog-fieldset
   #:create-fieldset
   ;; CLOG-Legend - Class for CLOG Legends
   #:clog-legend
   #:create-legend
   ;; CLOG-Form-Element - Class for form elements
   #:clog-form-element
   #:form-element-type
   #:create-form-element
   #:autocomplete
   #:autofocusp
   #:place-holder
   #:read-only-p
   #:requiredp
   #:multiplep
   #:name
   #:default-value
   #:radio-value
   #:checkbox-value
   #:select-value
   #:textarea-value
   #:name-value
   #:pattern
   #:size
   #:element-step
   #:select
   #:file-accept
   #:alt-text
   #:checkedp
   #:input-mode-type
   #:input-mode
   #:set-data-list
   #:make-data-list
   #:minimum-length
   #:maximum-length
   ;; CLOG-Label - Class for CLOG Labels
   #:clog-label
   #:create-label
   #:label-for
   ;; CLOG-Select - Class for CLOG Selects
   #:clog-select
   #:create-select
   #:clog-option
   #:create-option
   #:clog-optgroup
   #:create-optgroup
   #:selectedp
   #:add-select-option
   #:add-select-options
   #:add-select-optgroup
   #:select-text
   ;; CLOG-Data-List - Class for CLOG Option Data Lists
   #:clog-data-list
   #:create-data-list
   #:add-option
   #:add-options
   ;; CLOG-Text-Area - Class for CLOG Text Areas
   #:clog-text-area
   #:create-text-area
   #:word-wrap
   #:columns
   #:rows
   #:disable-resize
   ;;; CLOG Canvas Objects
   ;; CLOG-Canvas - Class for CLOG canvas objects
   #:clog-canvas
   #:create-canvas
   #:clog-context2d
   #:create-context2d
   ;; CLOG-Canvas - Properties
   #:fill-style
   #:canvas-filter
   #:font-style
   #:global-alpha
   #:global-composite-operation
   #:image-smoothing-enabled
   #:image-smoothing-quality
   #:line-cap
   #:line-dash-offset
   #:line-join
   #:line-width
   #:miter-limit
   #:shadow-blur
   #:shadow-color
   #:shadow-offset-x
   #:shadow-offset-y
   #:stroke-style
   #:text-align-type
   #:text-align
   #:text-baseline-type
   #:text-baseline
   #:text-dir
   ;; CLOG-Canvas - Methods
   #:arc
   #:arc-to
   #:begin-path
   #:bezier-curve-to
   #:clear-rect
   #:path-clip
   #:close-path
   #:create-image-data
   #:create-conic-gradient
   #:create-linear-gradient
   #:create-radial-gradient
   #:create-pattern
   #:draw-image
   #:draw-image-from-to
   #:ellipse
   #:path-fill
   #:fill-rect
   #:fill-text
   #:get-image-data
   #:get-line-dash
   #:get-transform
   #:is-point-in-path
   #:is-point-in-stroke
   #:line-to
   #:measure-text
   #:move-to
   #:put-image-data
   #:put-image-data-dirty
   #:quadratic-curve-to
   #:rect
   #:reset-transform
   #:canvas-restore
   #:rotate
   #:canvas-save
   #:scale
   #:set-line-dash
   #:set-transform
   #:set-transform-with-matrix
   #:path-stroke
   #:stroke-rect
   #:stroke-text
   #:transform
   #:translate
   ;; CLOG-Canvas-Gradient
   #:clog-canvas-gradient
   #:add-color-stop
   ;; CLOG-Image-Data
   #:clog-image-data
   #:json-image-data
   ;; CLOG-Matrix
   #:clog-matrix
   #:create-matrix
   #:flip-x
   #:flip-y
   #:inverse
   #:multiply
   #:scale-matrix
   #:scale3d
   #:translate-matrix
   ;; CLOG-Path2d
   #:clog-path2d
   #:create-path2d
   ;; CLOG-Text-Metrics
   #:clog-text-metrics
   #:actual-bounding-box-left
   #:actual-bounding-box-right
   #:actual-bounding-box-ascent
   #:actual-bounding-box-descent
   #:font-bounding-box-ascent
   #:font-bounding-box-descent
   #:em-height-ascent
   #:em-height-descent
   #:hanging-baseline
   #:alphabetic-baseline
   #:ideographic-baseline
   ;;; CLOG Multimedia Objects
   ;; CLOG-Multimedia - Base Class for CLOG multimedia objects
   #:clog-multimedia
   #:loop-mediap
   #:media-duration
   #:media-source
   #:media-position
   #:mutedp
   #:pausedp
   #:seekingp
   #:playback-ended-p
   #:playback-rate
   #:ready-to-play-p
   #:media-volume
   #:play-media
   #:pause-media
   #:load-media
   #:can-play-type-p
   ;; CLOG-Multimedia - Event Handlers
   #:set-on-media-abort
   #:set-on-media-error
   #:set-on-can-play
   #:set-on-can-play-through
   #:set-on-duration-change
   #:set-on-emptied
   #:set-on-ended
   #:set-on-loaded-data
   #:set-on-loaded-meta-data
   #:set-on-load-start
   #:set-on-play
   #:set-on-pause
   #:set-on-playing
   #:set-on-progress
   #:set-on-rate-change
   #:set-on-seeked
   #:set-on-seeking
   #:set-on-stalled
   #:set-on-suspend
   #:set-on-time-update
   #:set-on-volume-change
   #:set-on-waiting
   ;; Clog-Audio - Class for CLOG Audio Control
   #:clog-audio
   #:create-audio
   ;; Clog-Video - Class for CLOG Video Control
   #:clog-video
   #:create-video
   ;;; CLOG Body Objects
   ;; CLOG-Body - CLOG Body Objects
   #:clog-body
   ;; CLOG-Body - Properties
   #:run
   #:set-html-on-close
   #:window
   #:html-document
   #:location
   #:navigator
   ;;; CLOG Window Objects
   ;; CLOG Popups
   #:*clog-popup-path*
   #:enable-clog-popup
   #:open-clog-popup
   #:clog-popup-openned
   #:in-clog-popup-p
   ;; CLOG-Window - CLOG Window Objects
   #:clog-window
   ;; CLOG-Window - Properties
   #:window-name
   #:url-rewrite
   #:x-offset
   #:y-offset
   #:pixel-ratio
   #:screen-width
   #:screen-height
   #:screen-available-width
   #:screen-available-height
   #:screen-available-top
   #:screen-available-left
   #:screen-color-depth
   ;; CLOG-Window - Methods
   #:alert
   #:log-console
   #:log-error
   #:print-window
   #:scroll-by
   #:scroll-to
   #:open-window
   #:close-window
   #:close-connection
   #:request-animation-frame
   ;; CLOG-Window - Events
   #:set-on-abort
   #:set-on-error
   #:set-on-before-unload
   #:set-on-hash-change
   #:set-on-orientation-change
   #:set-on-animation-frame
   #:move-window-by
   #:move-window-to
   #:resize-by
   #:resize-to
   ;; CLOG-Window - History
   #:set-on-pop-state
   #:url-push-state
   ;; CLOG-Window - Storage Methods
   #:storage-type
   #:storage-length
   #:storage-key
   #:storage-remove
   #:storage-element
   ;; CLOG-Window - Storage Events
   #:set-on-storage
   ;;; CLOG Document Objects
   ;; CLOG-Document - CLOG Document Objects
   #:clog-document
   #:domain
   #:input-encoding
   #:last-modified
   #:referer
   #:title
   #:document-url
   #:head-element
   #:body-element
   #:document-element
   #:visibility-state
   #:ready-state
   #:load-css
   #:load-script
   #:put
   #:put-line
   #:put-br
   #:new-line
   ;; CLOG-Document - Events
   #:set-on-full-screen-change
   #:set-on-visibility-change
   #:set-on-ready-state-change
   #:set-on-load-script
   ;;; CLOG Navigator Objects
   ;; CLOG-Navigator - CLOG Navigator Objects
   #:clog-navigator
   ;; CLOG-Navigator - Properties
   #:cookie-enabled-p
   #:language
   #:user-agent
   #:vendor
   ;; CLOG-Navigator - Clipboard
   #:system-clipboard-write
   #:system-clipboard-read
   ;;; CLOG Location Objects
   ;; Clog-Location - CLOG Location Objects
   #:clog-location
   ;; CLOG-Location - Properties
   #:url
   #:hash
   #:host
   #:host-name
   #:origin
   #:path-name
   #:port
   #:protocol
   #:url-search
   ;; CLOG-Location - Methods
   #:reload
   #:url-replace
   #:url-assign))
