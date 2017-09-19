/*
module Home
( homeView
) where
*/

import * as Avers from 'avers'
import * as React from 'react'
import timeago from 'timeago.js'
import styled from 'styled-components'

import {App} from '../app'
import {Boulder} from '../storage'

import {BoulderCard} from './Components/BoulderCard'
import {Site} from './Components/Site'

export function
homeView(app: App) {
    const editableBoulders = app.data.activeBouldersCollection.ids.get([])
        .map(boulderId => Avers.lookupEditable<Boulder>(app.data.aversH, boulderId).get(null))
        .filter(x => x !== null)

    // Go through the list, render each boulder with <BoulderCard> and insert
    // headings in between two cards when the day they were created at changes.
    const res = editableBoulders.reduce(({boulders, date}, boulder) => {
        const objectId = boulder.objectId
        const createdAt = new Date(boulder.content.setDate)

        if (date === null) {
            return {
                boulders: boulders.concat([
                    <BoulderSeparator>{timeago().format(createdAt)}</BoulderSeparator>,
                    <BoulderCard key={objectId} app={app} boulderE={boulder} />,
                ]),
                date: createdAt,
            }
        } else if (date.getMonth() === createdAt.getMonth() && date.getDate() === createdAt.getDate()) {
            return {
                boulders: boulders.concat([<BoulderCard key={objectId} app={app} boulderE={boulder} />]),
                date: createdAt,
            }
        } else {
            return {
                boulders: boulders.concat([
                    <BoulderSeparator>{timeago().format(createdAt)}</BoulderSeparator>,
                    <BoulderCard key={objectId} app={app} boulderE={boulder} />,
                ]),
                date: createdAt,
            }
        }
    }, { boulders: [], date: null })

    return (
        <Site app={app}>
            <Boulders>
                {res.boulders}
            </Boulders>
        </Site>
    )
}


// ----------------------------------------------------------------------------
const Boulders = styled.div`
    margin-top: 1rem;
    display: flex;
    flex-direction: row;
    flex-wrap: wrap;
`

const BoulderSeparator = styled.div`
    flex: 0 0 100%;
    width: 100%;
    padding: 40px 16px 20px;
    font-size: 24px;
    line-height: 1;

    &:first-of-type {
        padding-top: 10px;
    }

    @media (min-width: 600px) {
        padding: 80px 24px 32px;
        font-size: 32px;
        &:first-of-type {
            padding: 20px 24px 32px;
        }
    }

    @media (min-width: 1200px) {
        padding: 120px 24px 60px;
        font-size: 44px;
        &:first-of-type {
            padding: 30px 24px 60px;
        }
    }
`
