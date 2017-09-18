import * as React from 'react'
import {Catalog, pageLoader} from 'catalog'

const pages = [
    {
        path: '/components/boulder-card',
        title: 'BoulderCard',
        component: pageLoader(() => import('./Components/BoulderCard.docs').then(x => x.default)),
    },
]

export function
catalogView() {
    return (
        <Catalog
            useBrowserHistory
            basePath='/_catalog'
            title='all-o-stasis'
            pages={pages}
        />
    )
}
