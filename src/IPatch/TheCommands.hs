module IPatch.TheCommands where

import Darcs.Commands ( CommandControl(CommandData, GroupName) )

import IPatch.Apply ( apply )
import IPatch.Split ( split )

commandControlList :: [CommandControl]
commandControlList =
    [ GroupName "Changing files"
    , CommandData apply
    , GroupName "Changing patches"
    , CommandData split
    ]

