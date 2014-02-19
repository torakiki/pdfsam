/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/feb/2014
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
        Button gitHubButton = createButton("http://www.pdfsam.org/scm", AwesomeIcon.GITHUB);
        gitHubButton.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Fork me on GitHub")));
        Button twitterButton = createButton("http://www.pdfsam.org/twitter", AwesomeIcon.TWITTER);
        twitterButton.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Follow us on Twitter")));
        Button facebookButton = createButton("http://www.pdfsam.org/facebook", AwesomeIcon.FACEBOOK);
        facebookButton.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Like us on Facebook")));
        Button gplusButton = createButton("http://www.pdfsam.org/gplus", AwesomeIcon.GOOGLE_PLUS);
        gplusButton.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Give us +1")));
        getChildren().addAll(gitHubButton, twitterButton, facebookButton, gplusButton);
        setVisible(false);
    }

    private Button createButton(String url, AwesomeIcon icon) {
        Button button = AwesomeDude.createIconButton(icon, null, "16px", "0px",
                ContentDisplay.GRAPHIC_ONLY);
        button.getStyleClass().addAll("pdfsam-toolbar-button", "quickbar-navigation-button");
        button.setOnAction(e -> eventStudio().broadcast(new OpenUrlRequestEvent(url)));
        return button;
    }
}
