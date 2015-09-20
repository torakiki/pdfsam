/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.io;

import static org.pdfsam.support.RequireUtils.requireNotNull;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.ui.support.Style;

import javafx.scene.control.CheckBox;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.VBox;

/**
 * Base panel with minimal output options
 * 
 * @author Andrea Vacondio
 * 
 */
class DestinationPane extends VBox {

    private CheckBox overwrite = new CheckBox(DefaultI18nContext.getInstance().i18n("Overwrite if already exists"));
    private BrowsableField destination;

    public DestinationPane(BrowsableField destination) {
        super(5);
        requireNotNull(destination, "Destination field cannot be null");
        this.destination = destination;
        overwrite.setSelected(false);
        overwrite.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                "Tick the box if you want to overwrite the output files if they already exist.")));

        destination.getStyleClass().addAll(Style.VITEM.css());
        getChildren().addAll(destination, overwrite);
        getStyleClass().addAll(Style.CONTAINER.css());
    }

    protected CheckBox overwrite() {
        return overwrite;
    }

    protected BrowsableField destination() {
        return destination;
    }
}
