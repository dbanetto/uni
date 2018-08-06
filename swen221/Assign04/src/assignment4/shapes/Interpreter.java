package assignment4.shapes;

import java.util.HashMap;
import java.util.Map;

/**
 * A Interpreter that processes a text input to produce shapes on a canvas
 */
public class Interpreter {
    private String input;
    private int index;
    private Canvas canvas;
    private Map<String, Shape> variables;

    /**
     * Create a Interpreter with a given input
     *
     * @param input to be interpreted
     */
    public Interpreter(String input) {
    	this.input = input.trim();
    	this.index = 0;
        canvas = null;
        variables = new HashMap<>();
    }

    /**
     * Create a Interpreter with a given input and a canvas to draw to
     *
     * @param input to be interpreted
     * @param canvas to be drawn to
     */
    public Interpreter(String input, Canvas canvas) {
        this.input = input.trim();
        this.index = 0;
        this.canvas = canvas;
        variables = new HashMap<>();
    }

    /**
     * This method return a canvas to which the input commands have been applied.
     *
     * @return a canvas that shows the result of the input.
     */
	public Canvas run() {
        // Operate on each line separately
        String[] lines = input.split("\n");
        for (String line : lines) {
            index = 0;
            input = line;
            while (index < input.length()) {
                evaluateNextCommand(canvas);
            }
        }
        input = "";
		return canvas;
 	}

    /**
     * This method returns a canvas to which the input commands have been applied.
     *
     * @param input command to be run
     * @return Canvas with the drawn results of the input
     */
    public Canvas run(String input) {
        this.input = input.trim();
        this.index = 0;
        return run();
    }

    /**
     * Evaluate the keyword at the start of the line
     *
     * moves index
     *
     * @param canvas to be drawn to
     */
	private void evaluateNextCommand(Canvas canvas) {
		this.canvas = canvas;
        skipWhiteSpace();
		String cmd = readWord();
		skipWhiteSpace();
		if (cmd.equals("fill")) {
            Color color = evaluateColor();
            // Only leave the shape expression
            input = input.substring(index, input.lastIndexOf('#'));
            index = 0;
            Shape shape = evaluate();
			fillShape(shape, color, this.canvas);
		} else if (cmd.equals("draw")) {
            Color color = evaluateColor();
            // Only leave the shape expression
            input = input.substring(index, input.lastIndexOf('#'));
            index = 0;
            Shape shape = evaluate();
            drawShape(shape, color, this.canvas);
        } else {
            skipWhiteSpace();
            if (input.charAt(index) == '=') { // is a variable declaration
                index++;
                variables.put(cmd, evaluate());
            } else {
                error();
            }
        }
	}

    /**
     * Iterate over word and bring index to end of word
     *
     * moves index
     *
     * @return word passed over
     */
    private String readWord() {
        int start = index;
        skipWhiteSpace();
        while(index < input.length() && Character.isLetter(input.charAt(index))) {
                index++;
        }
        return input.substring(start, index);
    }

    /**
     * Skip index pass whitespace
     *
     * moves index
     */
    private void skipWhiteSpace() {
        while (index < input.length()
               && (Character.isWhitespace(input.charAt(index)))) {
            index = index + 1;
        }
    }

    /**
     * Evaluate a shape constructor
     *
     * moves index
     *
     * @return A shape
     */
    private Shape evaluateConstruct() {
        skipWhiteSpace();
        while (index < input.length() && input.charAt(index) != '[') {
            index++;
        }
        if (index >= input.length()) {
            throw new IllegalArgumentException("Could not find \'[\'");
        }

        int endIndex = input.indexOf(']', index);
        if (endIndex == -1) {
            throw new IllegalArgumentException("Could not find \']\'");
        }
        if (endIndex - index == 1) {
            throw new IllegalArgumentException("No value for shape");
        }

        String[] shape = input.substring(index + 1, endIndex).split(",");
        if (shape.length == 4) {
            try {
                int x = Integer.parseInt(shape[0].trim());
                int y = Integer.parseInt(shape[1].trim());
                int w = Integer.parseInt(shape[2].trim());
                int h = Integer.parseInt(shape[3].trim());
                index = endIndex + 1; // ] character
                return new Rectangle(x, y, w, h);
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException("invalid rectangle " + e);
            }

        } else {
            throw new IllegalArgumentException("Unsupported shape of " + shape.length + " parts");
        }
    }

    /**
     * evaluate the Colour at the end of the line in
     * HTML Colour form e.g. #DEADBE
     *
     * Assumes the colour is at the end of the line
     *
     * @return Colour from hex representation
     */
    private Color evaluateColor() {
        // Jump to the back to where the Colour is
        int hashIndex = input.lastIndexOf('#');
        if (hashIndex == -1) {
            throw new IllegalArgumentException("Invalid colour: no start of colour");
        }

        String hex = input.substring(hashIndex);
        return new Color(hex);
    }

    /**
     * Fills the area of a shape will a given colour
     *
     * @param shape area to be filled
     * @param color what colour to be filled with
     * @param canvas where to draw it to
     */
    private void fillShape(Shape shape, Color color, Canvas canvas) {
        Rectangle bound = shape.boundingBox();

        // Make sure we can draw somewhere
        if (canvas == null) {
            canvas = new Canvas(bound.getX() + bound.getWidth(), bound.getY() + bound.getHeight(), Color.WHITE);
        }

        for (int x = bound.getX(); x <= bound.getX() + bound.getWidth(); x++) {
            for (int y = bound.getY(); y <= bound.getY() + bound.getHeight(); y++) {
                if (shape.contains(x, y)) {
                    canvas.draw(x, y, color);
                }
            }
        }
        this.canvas = canvas;
    }

    /**
     * Draw the outline of a shape
     * Uses scanline algorithm
     *
     * @param shape area to be filled
     * @param color what colour to be filled with
     * @param canvas where to draw it to
     */
    private void drawShape(Shape shape, Color color, Canvas canvas) {
        Rectangle bound = shape.boundingBox();

        // Make sure we can draw somewhere
        if (canvas == null) {
            canvas = new Canvas(bound.getX() + bound.getWidth(), bound.getY() + bound.getHeight(), Color.WHITE);
        }

        // Scan horizontal
        for (int x = bound.getX() - 1; x <= bound.getX() + bound.getWidth(); x++) {
            boolean isIn = false;
            for (int y = bound.getY() - 1; y <= bound.getY() + bound.getHeight(); y++) {
                boolean peek = shape.contains(x, y + 1);
                boolean last = shape.contains(x, y - 1);
                boolean current = shape.contains(x, y);
                if ((current != last && !isIn) || (current != peek && isIn)) {
                    isIn = current;

                        canvas.draw(x, y, color);

                }
            }
        }

        // Scan vertical
        for (int y = bound.getY() - 1; y <= bound.getY() + bound.getHeight(); y++) {
            boolean isIn = false;
            for (int x = bound.getX() - 1; x <= bound.getX() + bound.getWidth(); x++) {
                boolean peek = shape.contains(x + 1, y );
                boolean last = shape.contains(x - 1, y);
                boolean current = shape.contains(x, y);
                if ((current != last && !isIn) || (current != peek && isIn)) {
                    isIn = current;
                    canvas.draw(x, y, color);
                }
            }
        }
        this.canvas = canvas;
    }

    /**
     * Evaluate the shape expression
     *
     * moves index
     *
     * @return a evaluated shape
     */
    private Shape evaluate() {
        skipWhiteSpace();
        char lookAhead = input.charAt(index);
        Shape value = null;

        if (lookAhead == '(') {
            value = evaluateBrackets();
        } else if (Character.isLetterOrDigit(lookAhead)) {
            value = evaluateVariable();
        } else if (lookAhead == '[') {
            value = evaluateConstruct();
        } else {
            error();
        }

        skipWhiteSpace();

        if(index < input.length() && input.charAt(index) != ')') {
            lookAhead = input.charAt(index);

            // apply operators
            if(lookAhead == '+') {
                match("+");
                value = value.union(evaluate());
            } else if(lookAhead == '&') {
                match("&");
                value = value.intersected(evaluate());
            } else if(lookAhead == '-') {
                match("-");
                value = value.difference(evaluate());
            } else {
                error();
            }
        }

        return value;
    }

    /**
     * Handle brackets when evaluating shape
     *
     * moves index
     *
     * @return the evaluated shape inside the brackets
     */
    private Shape evaluateBrackets() {
        match("(");
        Shape value = evaluate();
        match(")");
        return value;
    }

    /**
     * Gets the value of a named shape
     * If the name is not assigned throws IllegalArgumentException
     *
     * moves index
     *
     * @return value of variable
     */
    private Shape evaluateVariable() {
        String var = "";
        while (index < input.length() && input.charAt(index) != ' ' && Character.isLetter(input.charAt(index))) {
            var += input.charAt(index);
            index++;
        }

        if (variables.containsKey(var)) {
            return variables.get(var);
        } else {
            throw new IllegalArgumentException(var + " variable does not exist");
        }
    }

    /**
     * Iterates forward until finds the given text
     *
     * moves index
     *
     * @param text to find
     */
    private void match(String text) {
        if(input.length() > index && input.startsWith(text, index)) {
            index += text.length();
        } else {
            error();
        }
    }

    /**
     * Throw IllegalArgumentException about the current index
     */
    private void error() {
        final String msg = "Cannot parse character '"
                + (input.length() > index ? input.charAt(index) : "\'Out of bounds\'")
                + "' at position " + index + " of input '" + input + "'\n";
        throw new IllegalArgumentException(msg);
    }
}
