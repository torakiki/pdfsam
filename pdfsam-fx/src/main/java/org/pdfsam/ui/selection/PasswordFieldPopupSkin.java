/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 04/mar/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
package org.pdfsam.ui.selection;


import javafx.scene.Node;
import javafx.scene.control.Skin;

/**
 * Skin for a {@link PasswordFieldPopup}
 * 
 * @author Andrea Vacondio
 *
 */
class PasswordFieldPopupSkin implements Skin<PasswordFieldPopup> {
    private PasswordFieldPopup popup;

    public PasswordFieldPopupSkin(final PasswordFieldPopup popup) {
        this.popup = popup;
        getNode().styleProperty().bind(popup.styleProperty());
        getNode().getStyleClass().addAll(popup.getStyleClass());
    }

    @Override
    public PasswordFieldPopup getSkinnable() {
        return popup;
    }

    @Override
    public Node getNode() {
        if (popup != null) {
            return popup.getPopupContent();
        }
        return null;
    }

    @Override
    public void dispose() {
        popup = null;
    }

}
