import Computation from 'computation'
import * as React from 'react'

import {markdown, ReactSpecimen} from '@catalog/core'

import {parseBoulderStat} from '../../../storage'

import {Visualization} from './Visualization'

const toEvents = (bss) => bss.map(bs => bs.removedOn === undefined
    ? [
        { bs, type: 'set', date: bs.setOn, setters: bs.setters, sector: bs.sector, grade: bs.grade },
      ]
    : [
        { bs, type: 'set', date: bs.setOn, setters: bs.setters, sector: bs.sector, grade: bs.grade },
        { bs, type: 'removed', date: bs.removedOn, setters: bs.setters, sector: bs.sector, grade: bs.grade },
      ],
    )
    .reduce((a, x) => a.concat(x), [])
    .sort((a, b) => +a.date - +b.date)
    .filter(a => a.date.getTime() > 10000)

const boulderStats = [{"setOn":"2017-09-24","removedOn":"1969-12-31","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"starship","grade":"blue"},{"setOn":"2017-10-29","removedOn":"2017-12-25","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"klagemauer","grade":"orange"},{"setOn":"2017-10-29","removedOn":null,"setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"dune","grade":"green"},{"setOn":"2017-10-29","removedOn":"2017-12-29","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"spektrumone","grade":"blue"},{"setOn":"2017-10-17","removedOn":"2017-10-29","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"starship","grade":"blue"},{"setOn":"2017-11-10","removedOn":"2017-11-10","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"dune","grade":"green"},{"setOn":"2017-10-29","removedOn":null,"setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"dune","grade":"blue"},{"setOn":"2017-10-29","removedOn":"2017-12-11","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"bigboss","grade":"orange"},{"setOn":"2017-09-18","removedOn":"1969-12-31","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"klagemauer","grade":"white"},{"setOn":"2017-12-26","removedOn":null,"setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"spektrumthree","grade":"blue"},{"setOn":"2017-12-26","removedOn":"2017-12-30","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"spektrumone","grade":"blue"},{"setOn":"2017-10-29","removedOn":null,"setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"klagemauer","grade":"yellow"},{"setOn":"2017-11-10","removedOn":"2017-11-10","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"starship","grade":"blue"},{"setOn":"2017-10-29","removedOn":null,"setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"spektrumone","grade":"white"},{"setOn":"2017-10-29","removedOn":"2017-11-30","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"dune","grade":"blue"},{"setOn":"2017-10-04","removedOn":null,"setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"klagemauer","grade":"orange"},{"setOn":"2017-12-26","removedOn":null,"setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"dune","grade":"green"},{"setOn":"2017-09-19","removedOn":"1969-12-31","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"spektrumthree","grade":"red"},{"setOn":"2017-10-29","removedOn":"2017-10-29","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"spektrumone","grade":"yellow"},{"setOn":"2017-10-29","removedOn":"2017-11-10","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"spektrumone","grade":"red"},{"setOn":"2017-12-24","removedOn":null,"setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"starship","grade":"blue"},{"setOn":"2017-10-10","removedOn":"2017-10-25","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"starship","grade":"yellow"},{"setOn":"2017-09-18","removedOn":"1969-12-31","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"spektrumone","grade":"orange"},{"setOn":"2017-12-21","removedOn":null,"setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"starship","grade":"blue"},{"setOn":"2017-09-18","removedOn":"1969-12-31","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"spektrumtwo","grade":"green"},{"setOn":"2017-09-18","removedOn":"1969-12-31","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"spektrumfour","grade":"yellow"},{"setOn":"2017-09-24","removedOn":"1969-12-31","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"kurswand","grade":"orange"},{"setOn":"2017-09-24","removedOn":"1969-12-31","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"starship","grade":"blue"},{"setOn":"2017-09-18","removedOn":"1969-12-31","setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"spektrumone","grade":"yellow"},{"setOn":"2017-10-18","removedOn":null,"setters":["NTTqDn8wIo98F95q4ehYPdgXL0H20H9mMbPkpYg5m9"],"sector":"starship","grade":"red"}]
    .map(parseBoulderStat)

export default () => markdown`
# \`<Visualization />\`

${<ReactSpecimen noSource>
    <div style={{height: 800, display: 'flex'}}>
        <Visualization
            bssC={Computation.pure(toEvents(boulderStats))}
            sectors={[]}
            selectedSetters={[]}
        />
    </div>
</ReactSpecimen>}
`
