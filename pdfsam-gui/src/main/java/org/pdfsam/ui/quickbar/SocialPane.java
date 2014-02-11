/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/feb/2014
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
package org.pdfsam.ui.quickbar;

import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.control.Button;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;

import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.OpenUrlRequestEvent;

import de.jensd.fx.fontawesome.AwesomeDude;
import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * Panel displaying social icons
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class SocialPane extends HBox {

    public SocialPane() {
        getStyleClass().add("quickbar-social");
        Button gitHubButton = AwesomeDude.createIconButton(AwesomeIcon.GITHUB, null, "20px", "0px",
                ContentDisplay.GRAPHIC_ONLY);
        gitHubButton.getStyleClass().addAll("pdfsam-toolbar-button", "quickbar-navigation-button");
        gitHubButton.setOnAction(e -> eventStudio().broadcast(new OpenUrlRequestEvent("http://www.pdfsam.org/scm")));
        gitHubButton.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Fork me on GitHub")));
        Button twitterButton = AwesomeDude.createIconButton(AwesomeIcon.TWITTER, null, "20px", "0px",
                ContentDisplay.GRAPHIC_ONLY);
        twitterButton.getStyleClass().addAll("pdfsam-toolbar-button", "quickbar-navigation-button");
        twitterButton.setOnAction(e -> eventStudio()
                .broadcast(new OpenUrlRequestEvent("http://www.pdfsam.org/twitter")));
        twitterButton.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Follow us on Twitter")));
        getChildren().addAll(gitHubButton, twitterButton);
        setVisible(false);
    }

}
