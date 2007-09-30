{-# OPTIONS -Wall -Werror #-}

-- |This module defines all of the data types that make up parsed
-- MUMPS syntax.

module HMumps.SyntaxTree (
                -- * Syntax-Tree Types
                -- ** Commands
                Command(..),
                EntryRef(..),
                FunArg(..),
                Vn(..),
                Label(..),
                Routineref(..),
                DoArg(..),
                ForArg(..),
                KillArg(..),
                GotoArg(..),
                MergeArg(..),
                NewArg(..),
                SetArg,
                WriteArg(..),
                WriteFormatCode(..),
                Name,
                -- ** Expressions
                Expression(..),
                Condition,
                Subscript,
                UnaryOp(..),
                BinOp(..),
               ) where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

import HMumps.Types
