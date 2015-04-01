/// <reference path="../../ext/react.d.ts" />

export interface NumberInputProps {
    object : any;
    field  : string;
}

interface NumberInputState {
    rawValue : string;
}

// The value is only valid if it can be fully parsed into a number.
function isValidNumber(value: string): boolean {
    var num = parseFloat(value);
    return !isNaN(num) && value == '' + num;
}

function asString(value) {
    if (value === null || value === undefined) {
        return '';
    } else {
        return value;
    }
}


class NumberInputSpec extends React.Component<NumberInputProps, NumberInputState> {

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
                if (isValidNumber(value)) {
                    this.props.object[this.props.field] = parseFloat(value);
                }
              }
            };

        var className = React.addons.classSet(
            { invalid : !isValidNumber(this.state.rawValue)
            }
        );

        function onClick(e) {
            e.stopPropagation();
        }

        return React.DOM.input
            ( { type: 'text', className: className, valueLink: valueLink, onClick: onClick }
            );
    }

    componentWillReceiveProps(props) {
        if (React.findDOMNode(this) !== document.activeElement) {
            this.setState(this.initialState(props));
        }
    }
}

export var NumberInput = React.createFactory(NumberInputSpec);
