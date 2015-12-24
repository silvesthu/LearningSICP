#lang scheme

; dead-lock possibly
; enter serialized exchange, then withdraw/deposit

; different layers should have different locks ?