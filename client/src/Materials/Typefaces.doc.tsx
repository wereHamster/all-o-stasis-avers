import * as React from 'react'

import {markdown, ReactSpecimen} from 'catalog'

import * as C from './Colors'
import * as T from './Typefaces'

export default () => markdown`
# Heading

${
<ReactSpecimen noSource plain>
<div>
<div style={T.h1 as any}>
h1. The quick brown fox jumps over the lazy dog
</div>
<div style={T.heading28 as any}>
heading28. The quick brown fox jumps over the lazy dog
</div>
<div style={T.heading24 as any}>
heading24. The quick brown fox jumps over the lazy dog
</div>
<div style={T.heading20 as any}>
heading20. The quick brown fox jumps over the lazy dog
</div>
<div style={T.heading18 as any}>
heading18. The quick brown fox jumps over the lazy dog
</div>
</div>
</ReactSpecimen>
}

# Copy

${
<ReactSpecimen noSource plain>
<div>
<div style={T.copy16 as any}>
copy16. The quick brown fox jumps over the lazy dog
</div>
<div style={T.copy16Bold as any}>
copy16Bold. The quick brown fox jumps over the lazy dog
</div>
<div style={T.copy14 as any}>
copy14. The quick brown fox jumps over the lazy dog
</div>
</div>
</ReactSpecimen>
}
`
