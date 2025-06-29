;; ArtisanCraft: Handmade Craft Authentication and Provenance Platform
;; Version: 1.0.0

(define-constant ERR-UNAUTHORIZED-ACCESS (err u1))
(define-constant ERR-CRAFT-NOT-FOUND (err u2))
(define-constant ERR-DUPLICATE-REGISTRATION (err u3))
(define-constant ERR-INVALID-AVAILABILITY (err u4))
(define-constant ERR-INVALID-CREATION-YEAR (err u5))
(define-constant ERR-INVALID-CRAFT-CATEGORY (err u6))
(define-constant ERR-INVALID-QUALITY-GRADE (err u7))
(define-constant ERR-INVALID-CRAFT-TITLE (err u8))
(define-constant ERR-INVALID-DESCRIPTION (err u9))

(define-constant MIN-CREATION-YEAR u1800)

(define-data-var next-craft-id uint u1)

(define-map artisan-crafts
    uint
    {
        artisan: principal,
        craft-title: (string-utf8 60),
        description: (string-utf8 250),
        craft-category: (string-utf8 20),
        quality-grade: (string-utf8 15),
        availability-status: (string-utf8 12),
        creation-year: uint
    }
)

(define-private (validate-craft-category (category (string-utf8 20)))
    (or 
        (is-eq category u"Pottery")
        (is-eq category u"Woodworking")
        (is-eq category u"Textiles")
        (is-eq category u"Metalwork")
        (is-eq category u"Glassblowing")
        (is-eq category u"Jewelry")
    )
)

(define-private (validate-quality-grade (grade (string-utf8 15)))
    (or 
        (is-eq grade u"Master")
        (is-eq grade u"Expert")
        (is-eq grade u"Skilled")
        (is-eq grade u"Apprentice")
        (is-eq grade u"Novice")
    )
)

(define-private (validate-string-length (text (string-utf8 250)) (min-len uint) (max-len uint))
    (let 
        (
            (text-len (len text))
        )
        (and 
            (>= text-len min-len)
            (<= text-len max-len)
        )
    )
)

(define-public (register-craft 
    (craft-title (string-utf8 60))
    (description (string-utf8 250))
    (craft-category (string-utf8 20))
    (quality-grade (string-utf8 15))
    (creation-year uint)
)
    (let
        (
            (craft-id (var-get next-craft-id))
        )
        (asserts! (validate-string-length craft-title u5 u60) ERR-INVALID-CRAFT-TITLE)
        (asserts! (validate-string-length description u15 u250) ERR-INVALID-DESCRIPTION)
        (asserts! (>= creation-year MIN-CREATION-YEAR) ERR-INVALID-CREATION-YEAR)
        (asserts! (validate-craft-category craft-category) ERR-INVALID-CRAFT-CATEGORY)
        (asserts! (validate-quality-grade quality-grade) ERR-INVALID-QUALITY-GRADE)
        
        (map-set artisan-crafts craft-id {
            artisan: tx-sender,
            craft-title: craft-title,
            description: description,
            craft-category: craft-category,
            quality-grade: quality-grade,
            availability-status: u"available",
            creation-year: creation-year
        })
        (var-set next-craft-id (+ craft-id u1))
        (ok craft-id)
    )
)

(define-public (mark-as-sold (craft-id uint))
    (let
        (
            (craft (unwrap! (map-get? artisan-crafts craft-id) ERR-CRAFT-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get artisan craft)) ERR-UNAUTHORIZED-ACCESS)
        (asserts! (is-eq (get availability-status craft) u"available") ERR-INVALID-AVAILABILITY)
        (ok (map-set artisan-crafts craft-id (merge craft { availability-status: u"sold" })))
    )
)

(define-read-only (get-craft-details (craft-id uint))
    (ok (map-get? artisan-crafts craft-id))
)

(define-read-only (get-artisan-address (craft-id uint))
    (ok (get artisan (unwrap! (map-get? artisan-crafts craft-id) ERR-CRAFT-NOT-FOUND)))
)