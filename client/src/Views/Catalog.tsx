import * as React from 'react'
import Loadable from 'react-loadable'

const LoadableCatalog = Loadable({
    loader: () => import('catalog'),
    render({Catalog, pageLoader}) {
        const pages = [
            {
                path: '/',
                title: 'Welcome',
                component: pageLoader(() => import('../../README.md')),
            },
            {
                path: '/materials',
                title: 'Materials',
                pages: [
                    {
                        path: '/materials/colors',
                        title: 'Colors',
                        component: pageLoader(() => import('../Materials/Colors.doc')),
                    },
                    {
                        path: '/materials/typfaces',
                        title: 'Typefaces',
                        component: pageLoader(() => import('../Materials/Typefaces.doc')),
                    },
                ],
            },
            {
                path: '/components',
                title: 'Components',
                pages: [
                    {
                        path: '/components/boulder-id',
                        title: 'BoulderId',
                        component: pageLoader(() => import('./Components/BoulderId.doc')),
                    },
                    {
                        path: '/components/boulder-card',
                        title: 'BoulderCard',
                        component: pageLoader(() => import('./Components/BoulderCard.doc')),
                    },
                    {
                        path: '/components/boulder-details',
                        title: 'BoulderDetails',
                        component: pageLoader(() => import('./Components/BoulderDetails.doc')),
                    },
                    {
                        path: '/components/setter-card',
                        title: 'SetterCard',
                        component: pageLoader(() => import('./Components/SetterCard.doc')),
                    },
                    {
                        path: '/components/sector-picker',
                        title: 'SectorPicker',
                        component: pageLoader(() => import('./Components/SectorPicker.doc')),
                    },
                    {
                        path: '/components/login',
                        title: 'Login',
                        component: pageLoader(() => import('./Login.doc')),
                    },
                ],
            },
        ]

        return (
            <Catalog
                useBrowserHistory
                basePath='/_catalog'
                title='all-o-stasis'
                pages={pages}
            />
        )
    },
    loading: () => <div />,
})

export function
catalogView() {
    return <LoadableCatalog />
}
