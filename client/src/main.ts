import page from 'page'

import * as Avers from 'avers'
import {Data, App, config, infoTable, refresh, loadView} from './app'

import {loadingView, notFoundView} from './views'

const mkApp = (): App => {
    const aversH = Avers.newHandle({
        apiHost: (config.secure ? "https://" : "http://") + config.apiHost,
        fetch: window.fetch.bind(window),
        createWebSocket: path => new WebSocket((config.secure ? "wss://" : "ws://") + config.apiHost + path),
        now: window.performance.now.bind(window.performance),
        infoTable,
    })

    const data = new Data(aversH)

    const containerElement = document.getElementById('root')
    if (!containerElement) {
        throw new Error('mkApp: #root not found')
    }

    return new App(
        containerElement,
        data,
        loadingView,
    )
}

const main = () => {
    // console.info('Starting app...')

    // Create the application instance. Pass all required configuration to the
    // constructor.
    const app = mkApp()

    // Attach a listener to watch for changes. Refresh the application UI
    // when any change happens. This is the main rendering loop of the
    // application. The 'console.info' shows you how frequently the application
    // data changes.
    Avers.attachGenerationListener(app.data.aversH, () => {
        // console.info('Generation', app.data.aversH.generationNumber)
        refresh(app)
    })

    // This template uses page.js for the router. If you target modern browsers
    // you may get away with a more straightforward implementation and listen
    // to the hashchange events directly.
    setupRoutes(app)

    // The app template makes use of the Avers Session. First thing we try to
    // do is to restore an existing session. The views will use the information
    // that is stored in the session object to determine what to show.
    Avers.restoreSession(app.data.session)

    // Expose some useful tools on 'window' to make them easily accessible from
    // the developer console. This part is entirely optional, though
    // I recommend to keep it even in production. It doesn't add any significant
    // overhead and gives you easy access to the application state.
    const windowAny = window as any
    windowAny.Avers = Avers
    windowAny.app = app
}


function setupRoutes(app: App) {
    page('/', async () => {
        const { homeView } = await import(/* webpackChunkName: "home" */ "./Views/Home")
        loadView(app, homeView)
    })

    page('/login', async () => {
        const { loginView } = await import(/* webpackChunkName: "login" */ "./Views/Login")
        loadView(app, loginView)
    })

    page('/sector', async () => {
        const { sectorView } = await import(/* webpackChunkName: "sector" */ "./Views/Sector")
        loadView(app, sectorView)
    })

    page('/stats', async () => {
        const { statsView } = await import(/* webpackChunkName: "stats" */ "./Views/Stats")
        loadView(app, statsView)
    })

    page('/boulder/:boulderId', async ctx => {
        const { boulderView } = await import(/* webpackChunkName: "boulder" */ "./Views/Boulder")
        loadView(app, boulderView(ctx.params.boulderId))
    })

    page('/account/:accountId', async ctx => {
        const { accountView } = await import(/* webpackChunkName: "account" */ "./Views/Account")
        loadView(app, accountView(ctx.params.accountId))
    })

    page('/team', async () => {
        const { teamView } = await import(/* webpackChunkName: "team" */ "./Views/Team")
        loadView(app, teamView)
    })

    page('/email-confirmed', async () => {
        const { emailConfirmedView } = await import(/* webpackChunkName: "email-confirmed" */ "./Views/EmailConfirmed")
        loadView(app, emailConfirmedView)
    })

    page('/_catalog', async () => {
        const { catalogView } = await import(/* webpackChunkName: "catalog" */ './Views/Catalog');
        loadView(app, catalogView)
    })
    page('/_catalog/*', async () => {
        const { catalogView } = await import(/* webpackChunkName: "catalog" */ './Views/Catalog');
        loadView(app, catalogView)
    })

    page('/loading', () => {
        loadView(app, loadingView)
    })

    page('*', () => {
        loadView(app, notFoundView)
    })

    page()
}

main()
