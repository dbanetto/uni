package RobotParser;

public interface ProgramObject extends Cloneable, Expression {
    ProgramObject clone();
    Object getValue();
}