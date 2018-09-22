import * as React from 'react'

import {markdown, ReactSpecimen} from 'catalog'

import {BoulderId, BoulderId24} from './BoulderId'

export default () => markdown`
# \`<BoulderId />\`

This version increasese in size as the viewport size increases.

${<ReactSpecimen noSource span={1}>
    <BoulderId grade='yellow'>3</BoulderId>
</ReactSpecimen>}
${<ReactSpecimen noSource span={1}>
    <BoulderId grade='green'>5</BoulderId>
</ReactSpecimen>}
${<ReactSpecimen noSource span={1}>
    <BoulderId grade='orange'>11</BoulderId>
</ReactSpecimen>}
${<ReactSpecimen noSource span={1}>
    <BoulderId grade='blue'>47</BoulderId>
</ReactSpecimen>}
${<ReactSpecimen noSource span={1}>
    <BoulderId grade='red'>99</BoulderId>
</ReactSpecimen>}
${<ReactSpecimen noSource span={1}>
    <BoulderId grade='white'>26</BoulderId>
</ReactSpecimen>}

# \`<BoulderId24 />\`

Fixed-size version of \`<BoulderId />\`.

${<ReactSpecimen noSource span={1}>
    <BoulderId24 grade='white'>26</BoulderId24>
</ReactSpecimen>}
`
