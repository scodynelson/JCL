package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.lists.ListStruct;

import java.util.List;

public class LetEnvironmentListStruct extends ListStruct {

	private final Environment environment;
	private final ListStruct listStruct;

	public LetEnvironmentListStruct(final Environment environment, final ListStruct listStruct) {
		super(null, null);
		this.environment = environment;
		this.listStruct = listStruct;
	}

	public Environment getEnvironment() {
		return environment;
	}

	public ListStruct getListStruct() {
		return listStruct;
	}

	@Override
	public int size() {
		return listStruct.size();
	}

	@Override
	public LispStruct getFirst() {
		return listStruct.getFirst();
	}

	@Override
	public ListStruct getRest() {
		return listStruct.getRest();
	}

	@Override
	public LispStruct getElement(final int index) {
		return listStruct.getElement(index);
	}

	@Override
	public void setElement(final int index, final LispStruct newValue) {
		listStruct.setElement(index, newValue);
	}

	@Override
	public List<LispStruct> getAsJavaList() {
		return listStruct.getAsJavaList();
	}

	@Override
	public boolean isDotted() {
		return listStruct.isDotted();
	}

	@Override
	public boolean isCircular() {
		return listStruct.isCircular();
	}
}
