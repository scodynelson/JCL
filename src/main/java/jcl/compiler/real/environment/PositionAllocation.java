package jcl.compiler.real.environment;

public abstract class PositionAllocation implements Allocation {

	private int position;

	protected PositionAllocation(final int position) {
		this.position = position;
	}

	public int getPosition() {
		return position;
	}

	public void setPosition(final int position) {
		this.position = position;
	}
}
