/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/nov/2013
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import static org.pdfsam.ui.help.HelpUtils.helpIcon;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.ui.ResettableView;
import org.pdfsam.ui.support.Style;

import javafx.scene.control.CheckBox;
import javafx.scene.layout.VBox;

/**
 * Base panel with minimal output options
 * 
 * @author Andrea Vacondio
 * 
 */
class DestinationPane extends VBox implements ResettableView {

    private CheckBox overwrite = new CheckBox(DefaultI18nContext.getInstance().i18n("Overwrite if already exists"));
    private BrowsableField destination;

    public DestinationPane(BrowsableField destination) {
        super(Style.DEFAULT_SPACING);
        requireNotNullArg(destination, "Destination field cannot be null");
        this.destination = destination;
        overwrite.setSelected(false);
        overwrite.setGraphic(helpIcon(DefaultI18nContext.getInstance()
                .i18n("Tick the box if you want to overwrite the output files if they already exist.")));
        overwrite.getStyleClass().addAll(Style.WITH_HELP.css());

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

    @Override
    public void resetView() {
        overwrite.setSelected(false);
        destination.getTextField().setText("");
    }
}
