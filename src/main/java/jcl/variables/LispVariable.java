package jcl.variables;

@FunctionalInterface
public interface LispVariable<T> {

	T getValue();
}
