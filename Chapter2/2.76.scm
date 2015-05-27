#lang scheme

; Explicit dispatch

	; Add type

		; Add implementation
		; Add branch in generic interface

	; Add op

		; Add implementation
		; Add new generic interface

; Data-directed style

	; Add type

		; Add package of functions and a installer

	; Add op

		; Add generic interface, and implementation in each package

; Message-passing-style

	; Add type

		; Write a new constructor

	; Add op

		; Add new branch of cond in exist constructor

; -------------

; Best for adding type often : Message-passing-style

; Best for adding op often : Data-directed style ? (or Message-passing-style...)