/*
 * Created on 04/feb/2013
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.gui;

import java.awt.BorderLayout;
import java.awt.Component;

import javax.swing.JPanel;

import bibliothek.gui.dock.FlapDockStation.Direction;
import bibliothek.gui.dock.common.CControl;
import bibliothek.gui.dock.common.CMinimizeArea;
import bibliothek.gui.dock.common.CStation;
import bibliothek.gui.dock.common.CStationContainer;
import bibliothek.gui.dock.common.CStationContainerListener;
import bibliothek.gui.dock.common.CWorkingArea;
import bibliothek.gui.dock.common.mode.ExtendedMode;
import bibliothek.util.Path;

/**
 * Content area to use as container for DockingFrames. It has a central {@link CWorkingArea} and a bottom {@link CMinimizeArea.
 * @author Andrea Vacondio
 *
 */
public class PdfsamContentArea extends JPanel implements CStationContainer {

    public static final Path TYPE_ID_CENTER = new Path("dock", "PdfsamContentArea", "center");
    public static final Path TYPE_ID_MINIMIZE = new Path("dock", "PdfsamContentArea", "minimize");

    private CWorkingArea center;
    private CMinimizeArea bottom;
    private CControl control;

    private String uniqueId;
    private CStation<?>[] stations;

    public PdfsamContentArea(CControl control, String uniqueId) {
        this.control = control;
        this.uniqueId = uniqueId;

        center = new CWorkingArea(control, uniqueId + " center");
        bottom = new CMinimizeArea(control, uniqueId + " south");

        center.getStation().setExpandOnDoubleclick(false);

        bottom.setDirection(Direction.NORTH);

        setLayout(new BorderLayout());
        add(center.getStation(), BorderLayout.CENTER);
        add(bottom, BorderLayout.SOUTH);

        stations = new CStation[] { bottom, center };
    }

    @Override
    public void addStationContainerListener(CStationContainerListener listener) {
        // nothing
    }

    @Override
    public void removeStationContainerListener(CStationContainerListener listener) {
        // nothing
    }

    @Override
    public String getUniqueId() {
        return uniqueId;
    }

    @Override
    public Component getComponent() {
        return this;
    }

    public int getStationCount() {
        return stations.length;
    }

    public CStation<?> getStation(int index) {
        return stations[index];
    }

    @Override
    public CStation<?> getDefaultStation() {
        return center;
    }

    @Override
    public CStation<?> getDefaultStation(ExtendedMode mode) {
        if (mode == ExtendedMode.MINIMIZED) {
            return bottom;
        }
        if (mode == ExtendedMode.EXTERNALIZED) {
            return null;
        }
        return center;
    }

    public int indexOf(CStation<?> child) {
        for (int i = 0; i < stations.length; i++) {
            if (stations[i] == child) {
                return i;
            }
        }
        return -1;
    }

    @Override
    public CStation<?> getMatchingStation(CStationContainer container, CStation<?> station) {
        if (container instanceof PdfsamContentArea) {
            PdfsamContentArea other = (PdfsamContentArea) container;
            if (other.getStationCount() == getStationCount()) {
                return getStation(other.indexOf(station));
            }
        }
        return null;
    }

    public CWorkingArea getWorkingArea() {
        return center;
    }

    public CMinimizeArea getMinimizeArea() {
        return bottom;
    }

    public CControl getControl() {
        return control;
    }
}
