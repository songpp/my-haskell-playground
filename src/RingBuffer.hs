{-# LANGUAGE GADTs #-}

module RingBuffer where


data RingBuffer a



new :: RingBuffer a
new size = undefined

append  :: a -> RingBuffer a -> RingBuffer a
append = undefined

isFull :: RingBuffer a -> Bool
isFull = undefined

