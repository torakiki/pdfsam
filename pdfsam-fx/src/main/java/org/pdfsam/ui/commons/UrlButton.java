/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ott/2013
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
package org.pdfsam.ui.commons;

import static java.util.Objects.nonNull;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import org.pdfsam.ui.support.Style;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import de.jensd.fx.glyphs.fontawesome.utils.FontAwesomeIconFactory;
import javafx.scene.control.Button;

/**
 * Button opening the default browser to the configured url when pressed
 * 
 * @author Andrea Vacondio
 * 
 */
public class UrlButton extends Button {

    private UrlButton(String text) {
        super(text);
    }

    /**
     * factory methods to create an url button with default {@link Style#BUTTON} style
     * 
     * @param text
     *            optional button text
     * @param url
     * @param icon
     *            optional icon
     * @return
     */
    public static final UrlButton styledUrlButton(String text, String url, FontAwesomeIcon icon) {
        return urlButton(text, url, icon, Style.BUTTON.css());
    }

    /**
     * Factory method to create an {@link UrlButton}
     * 
     * @param text
     *            optional button text
     * @param url
     * @param icon
     *            optional icon
     * @param style
     *            optional style classes
     * @return
     */
    public static final UrlButton urlButton(String text, String url, FontAwesomeIcon icon, String... style) {
        requireNotBlank(url, "URL cannot be blank");
        UrlButton button = new UrlButton(text);
        button.setOnAction(e -> eventStudio().broadcast(new OpenUrlRequest(url)));
        if (nonNull(icon)) {
            FontAwesomeIconFactory.get().setIcon(button, icon);
        }
        if (nonNull(style) && style.length > 0) {
            button.getStyleClass().addAll(style);
        }
        return button;
    }

}
