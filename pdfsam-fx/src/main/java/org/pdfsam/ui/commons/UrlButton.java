/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/ott/2013
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
package org.pdfsam.ui.commons;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.support.RequireUtils.require;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.control.Button;

import org.pdfsam.ui.support.Style;

/**
 * Button opening the default browser to the configured url when pressed
 * 
 * @author Andrea Vacondio
 * 
 */
public class UrlButton extends Button {

    public UrlButton(String text, String url) {
        super(text);
        require(isNotBlank(url), "URL cannot be blank");
        setOnAction(e -> eventStudio().broadcast(new OpenUrlRequest(url)));
        // not sure about this. see: https://javafx-jira.kenai.com/browse/RT-28779
        /**
         * setOnKeyReleased(new EventHandler<KeyEvent>() { final KeyCombination combo = new KeyCodeCombination(KeyCode.ENTER);
         * 
         * public void handle(KeyEvent t) { if (combo.match(t)) { openUrl(); } } });
         */
        getStyleClass().addAll(Style.BUTTON.css());
    }
}
