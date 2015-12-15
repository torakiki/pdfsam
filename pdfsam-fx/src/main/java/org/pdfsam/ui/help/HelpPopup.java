/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15 dic 2015
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.help;

import javafx.scene.Node;
import javafx.scene.control.PopupControl;
import javafx.scene.control.Skin;
import javafx.scene.layout.Region;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;
import javafx.stage.Screen;

/**
 * A simple popup that shows help hints
 * 
 * @author Andrea Vacondio
 *
 */
public class HelpPopup extends PopupControl {
    private Region content;

    public HelpPopup(String message) {
        this(new TextFlow(new Text(message)));
    }

    public HelpPopup(Region content) {
        getStyleClass().setAll("pdfsam-help-popup");
        setAutoHide(true);
        setHideOnEscape(true);
        setAutoFix(true);
        this.content = content;
        this.content.setMaxWidth(Screen.getPrimary().getVisualBounds().getWidth() / 3);
    }

    Node getPopupContent() {
        return content;
    }

    @Override
    protected Skin<?> createDefaultSkin() {
        return new HelpPopupSkin(this);
    }
}
