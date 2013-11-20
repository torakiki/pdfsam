/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/set/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.event;

import static org.pdfsam.support.RequireUtils.requireNotNull;
import javafx.scene.Node;

/**
 * Callback to enable or disable a given component.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class EnableDisableComponentCallback implements EventSubscriberCallback {

    private Node comp;
    private boolean disable = false;

    private EnableDisableComponentCallback(Node comp, boolean disable) {
        requireNotNull(comp, "Component cannot be null");
        this.comp = comp;
        this.disable = disable;
    }

    @Override
    public void exec(ModuleEvent e) {
        comp.setDisable(disable);
    }

    /**
     * @param comp
     * @return a callback that disables the given component
     */
    public static EventSubscriberCallback disableComponent(Node comp) {
        return new EnableDisableComponentCallback(comp, true);
    }

    /**
     * @param comp
     * @return a callback that enables the given component
     */
    public static EventSubscriberCallback enableComponent(Node comp) {
        return new EnableDisableComponentCallback(comp, false);
    }
}
