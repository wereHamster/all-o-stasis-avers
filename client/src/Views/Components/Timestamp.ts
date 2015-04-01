
export interface TimestampProps {
    object : any;
    field  : string;
}

interface TimestampState {
    rawValue : string;
}

function isValidTimestamp(value: string): boolean {
    var parsed = parseTimestamp(value);
    return parsed !== undefined;
}

function parseTimestamp(value: string): string {
    var d = Date.parse(value);
    if (!isNaN(d)) {
        return new Date(d).toISOString();
    }
}

function asString(value): string {
    if (value === null || value === undefined) {
        return '';
    } else {
        return value;
    }
}

class TimestampSpec extends React.Component<TimestampProps, TimestampState> {

    initialState(props) {
        var rawValue = props.object[props.field];
        return { rawValue : asString(rawValue) };
    }

    constructor(props) {
        super(props);
        this.state = this.initialState(props);
    }

    render() {
        var valueLink =
            { value: this.state.rawValue
            , requestChange: value => {
                this.setState({ rawValue: value });
                var parsed = parseTimestamp(value);
                if (isValidTimestamp(value)) {
                    this.props.object[this.props.field] = parsed;
                }
              }
            };

        var className = React.addons.classSet(
            { invalid : !isValidTimestamp(this.state.rawValue)
            , wide    : true
            }
        );

        function onClick(e) {
            e.stopPropagation();
        }

        return React.DOM.input
            ( { type: 'text', className: className, valueLink: valueLink, onClick: onClick }
            );
    }
}

export var Timestamp = React.createFactory(TimestampSpec);
