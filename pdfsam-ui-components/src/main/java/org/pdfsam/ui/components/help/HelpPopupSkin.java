/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15 dic 2015
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
package org.pdfsam.ui.components.help;

import static java.util.Optional.ofNullable;

import javafx.scene.Node;
import javafx.scene.control.Skin;

/**
 * Skin for an {@link HelpPopup}
 * 
 * @author Andrea Vacondio
 *
 */
public class HelpPopupSkin implements Skin<HelpPopup> {
    private HelpPopup popup;

    public HelpPopupSkin(final HelpPopup popup) {
        this.popup = popup;
        getNode().styleProperty().bind(popup.styleProperty());
        getNode().getStyleClass().addAll(popup.getStyleClass());
    }

    @Override
    public HelpPopup getSkinnable() {
        return popup;
    }

    @Override
    public Node getNode() {
        return ofNullable(popup).map(HelpPopup::getPopupContent).orElse(null);
    }

    @Override
    public void dispose() {
        popup = null;
    }

}
