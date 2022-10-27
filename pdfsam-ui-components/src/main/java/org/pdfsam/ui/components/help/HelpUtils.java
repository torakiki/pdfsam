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

import javafx.geometry.Point2D;
import javafx.scene.layout.Region;
import javafx.scene.text.Text;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;

/**
 * @author Andrea Vacondio
 *
 */
public final class HelpUtils {

    private HelpUtils() {
        // hide
    }

    /**
     * Creates an help icon that shows a popup with the given message
     *
     * @param text
     * @return the help icon
     */
    public static Text helpIcon(String text) {
        HelpPopup popup = new HelpPopup(text);
        return helpIcon(popup);
    }

    /**
     * Creates an help icon that shows a popup with the given content
     *
     * @return the help icon
     */
    public static Text helpIcon(Region content) {
        HelpPopup popup = new HelpPopup(content);
        return helpIcon(popup);
    }

    private static Text helpIcon(HelpPopup popup) {
        var icon = FontIcon.of(UniconsLine.QUESTION_CIRCLE);
        icon.getStyleClass().add("help-icon");
        icon.setOnMouseEntered(e -> {
            Point2D p = icon.localToScreen(icon.getLayoutBounds().getMaxX(), icon.getLayoutBounds().getMaxY());
            popup.show(icon, p.getX(), p.getY());
            e.consume();
        });
        icon.setOnMouseExited(e -> popup.hide());
        return icon;
    }
}
