import * as React from 'react'

import {markdown, ReactSpecimen} from 'catalog'

import * as C from './Colors'
import * as T from './Typefaces'

export default () => markdown`
# Heading

${
<ReactSpecimen noSource>
<div style={T.h1 as any}>
The quick brown fox jumps over the lazy dog
</div>
</ReactSpecimen>
}

# Copy

${
<ReactSpecimen noSource>
<div style={T.copy16 as any}>
The quick brown fox jumps over the lazy dog
</div>
</ReactSpecimen>
}

${
<ReactSpecimen noSource>
<div style={T.copy14 as any}>
The quick brown fox jumps over the lazy dog
</div>
</ReactSpecimen>
}
`
