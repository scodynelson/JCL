package jcl.compiler.real.environment;

import java.util.List;

public class Environment {

	public static final Environment FREE = null;
	public static final Environment NULL = null;

	private Marker marker;
	private Object loadTimeValue;
	private Environment parent;
	private List<Binding> bindings;
	private SymbolTable symbolTable;
	private Closure environmentClosure;

	// TODO: load-time-value ???
	public Environment(final Marker marker, final Object loadTimeValue, final Environment parent, final List<Binding> bindings,
					   final SymbolTable symbolTable, final Closure environmentClosure) {
		this.marker = marker;
		this.parent = parent;
		this.loadTimeValue = loadTimeValue;
		this.bindings = bindings;
		this.symbolTable = symbolTable;
		this.environmentClosure = environmentClosure;
	}

	public Marker getMarker() {
		return marker;
	}

	public Object getLoadTimeValue() {
		return loadTimeValue;
	}

	public Environment getParent() {
		return parent;
	}

	public List<Binding> getBindings() {
		return bindings;
	}

	public SymbolTable getSymbolTable() {
		return symbolTable;
	}

	public Closure getEnvironmentClosure() {
		return environmentClosure;
	}

	public void setMarker(final Marker marker) {
		this.marker = marker;
	}

	public void setLoadTimeValue(final Object loadTimeValue) {
		this.loadTimeValue = loadTimeValue;
	}

	public void setParent(final Environment parent) {
		this.parent = parent;
	}

	public void setBindings(final List<Binding> bindings) {
		this.bindings = bindings;
	}

	public void setSymbolTable(final SymbolTable symbolTable) {
		this.symbolTable = symbolTable;
	}

	public void setEnvironmentClosure(final Closure environmentClosure) {
		this.environmentClosure = environmentClosure;
	}
}
