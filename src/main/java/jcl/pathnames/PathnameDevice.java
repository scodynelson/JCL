package jcl.pathnames;

import org.apache.commons.lang3.StringUtils;

/**
 * The {@code PathnameDevice} is the object representation of the 'device' element of a Lisp 'pathname' type.
 */
public final class PathnameDevice {

	private final String device;
	private final PathnameComponentType componentType;

	/**
	 * Public constructor.
	 */
	public PathnameDevice() {
		device = null;
		componentType = PathnameComponentType.UNSPECIFIC;
	}

	/**
	 * Public constructor.
	 *
	 * @param device the pathname device
	 */
	public PathnameDevice(final String device) {
		this.device = device;

		if (StringUtils.isEmpty(device)) {
			componentType = PathnameComponentType.NIL;
		} else if ("*".equals(device)) {
			componentType = PathnameComponentType.WILD;
		} else {
			componentType = null;
		}
	}

	/**
	 * Public constructor.
	 *
	 * @param componentType pathname device component type
	 */
	public PathnameDevice(final PathnameComponentType componentType) {
		device = null;
		this.componentType = componentType;
	}

	/**
	 * Getter for pathname device value.
	 *
	 * @return pathname device value
	 */
	public String getDevice() {
		return device;
	}

	/**
	 * Getter for pathname device component type.
	 *
	 * @return pathname device component type
	 */
	public PathnameComponentType getComponentType() {
		return componentType;
	}

	@Override
	public String toString() {
		return "PathnameDevice{"
				+ "device=" + device
				+ ", componentType=" + componentType
				+ '}';
	}
}
