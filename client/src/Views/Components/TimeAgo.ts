
export interface TimeAgoProps {
    live        ?: boolean;
    component   ?: string;
    formatter?;
    date      : string;
}

export interface TimeAgoState {
    timerId: any;
}


function tick(self, refresh: boolean) {
    var period = 1000;

    var then = (new Date(self.props.date)).valueOf();
    var now = Date.now();
    var seconds = Math.round(Math.abs(now-then)/1000);

    if (seconds < 60) {
        period = 1000;
    } else if(seconds < 60*60) {
        period = 1000 * 60;
    } else if(seconds < 60*60*24) {
        period = 1000 * 60 * 60;
    } else {
        period = 0;
    }

    if (!!period) {
        self.setState({
            timerId: setTimeout(function() {
                tick(self, false);
            }, period)
        });
    }

    if (!refresh) {
        self.forceUpdate();
    }
}


export class TimeAgoSpec extends React.Component<TimeAgoProps, TimeAgoState> {

    static defaultProps =
        { live: true
        , component: 'span'
        , formatter: function (value, unit, suffix) {
            if (value !== 1) {
                unit += 's';
            }
            return value + ' ' + unit + ' ' + suffix;
          }
        , date: new Date(0).toISOString()
        };

    constructor(props) {
        super(props);
        this.state = { timerId: null };
    }

    componentDidMount() {
        if (this.props.live) {
            tick(this, true);
        }
    }

    componentWillUnmount() {
        clearTimeout(this.state.timerId);
    }


    render() {
        var then = (new Date(this.props.date)).valueOf();
        var now = Date.now();
        var seconds = Math.round(Math.abs(now-then)/1000);

        var suffix = then < now ? 'ago' : 'from now';

        var value;
        var unit;

        if (seconds < 60) {
            value = Math.round(seconds);
            unit = 'second';
        } else if (seconds < 60*60) {
            value = Math.round(seconds/60);
            unit = 'minute';
        } else if (seconds < 60*60*24) {
            value = Math.round(seconds/(60*60));
            unit = 'hour';
        } else if (seconds < 60*60*24*7) {
            value = Math.round(seconds/(60*60*24));
            unit = 'day';
        } else if (seconds < 60*60*24*30) {
            value = Math.round(seconds/(60*60*24*7));
            unit = 'week';
        } else if (seconds < 60*60*24*365) {
            value = Math.round(seconds/(60*60*24*30));
            unit = 'month';
        } else {
            value = Math.round(seconds/(60*60*24*365));
            unit = 'year';
        }

        var props =
            { live : this.props.live
            };

        return React.createElement(this.props.component, props, this.props.formatter(value, unit, suffix));
    }
}

export var TimeAgo = React.createFactory(TimeAgoSpec);
