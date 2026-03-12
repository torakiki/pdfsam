/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/01/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.components.sidebar;

import javafx.scene.Node;

/**
 * {@link SidebarButtonWithNotification} that can also be selected.
 *
 * @author Andrea Vacondio
 */
public class SelectableSidebarButtonWithNotification extends SidebarButtonWithNotification<SelectableSidebarButton>
        implements Selectable {

    public SelectableSidebarButtonWithNotification(SelectableSidebarButton wrapped, Node notificationGraphic,
            SidebarNotificationType notificationType) {
        super(wrapped, notificationGraphic, notificationType);
    }

    @Override
    public void setSelected(boolean value) {
        this.getWrapped().setSelected(value);
    }

    @Override
    public boolean isSelected() {
        return this.getWrapped().isSelected();
    }

    @Override
    public void selectIf(String id) {
        this.getWrapped().selectIf(id);
    }

}
