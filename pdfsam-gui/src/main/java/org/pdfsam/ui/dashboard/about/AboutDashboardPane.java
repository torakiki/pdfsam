/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/ott/2013
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
package org.pdfsam.ui.dashboard.about;

import static org.pdfsam.ConfigurableProperty.COPYRIGHT;
import static org.pdfsam.ConfigurableProperty.DOCUMENTATION_URL;
import static org.pdfsam.ConfigurableProperty.DONATE_URL;
import static org.pdfsam.ConfigurableProperty.FACEBOOK_URL;
import static org.pdfsam.ConfigurableProperty.FEED_URL;
import static org.pdfsam.ConfigurableProperty.HOME_LABEL;
import static org.pdfsam.ConfigurableProperty.HOME_URL;
import static org.pdfsam.ConfigurableProperty.LICENSE_NAME;
import static org.pdfsam.ConfigurableProperty.LICENSE_URL;
import static org.pdfsam.ConfigurableProperty.SCM_URL;
import static org.pdfsam.ConfigurableProperty.SUPPORT_URL;
import static org.pdfsam.ConfigurableProperty.TRACKER_URL;
import static org.pdfsam.ConfigurableProperty.TRANSLATE_URL;
import static org.pdfsam.ConfigurableProperty.TWITTER_URL;
import static org.pdfsam.ConfigurableProperty.VERSION;
import static org.pdfsam.support.io.ObjectCollectionWriter.writeContent;

import java.util.Arrays;

import javax.inject.Inject;

import org.apache.commons.io.FileUtils;
import org.pdfsam.Pdfsam;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.ui.commons.UrlButton;
import org.pdfsam.ui.support.Style;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import de.jensd.fx.glyphs.fontawesome.utils.FontAwesomeIconFactory;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

/**
 * Panel displaying PDFsam logo and About panel informations
 * 
 * @author Andrea Vacondio
 * 
 */
public class AboutDashboardPane extends HBox {

    @Inject
    public AboutDashboardPane(Pdfsam pdfsam) {
        getStyleClass().add("dashboard-container");
        VBox left = new VBox(5);
        addSectionTitle(pdfsam.name(), left);
        Label copyright = new Label(pdfsam.property(COPYRIGHT));
        FontAwesomeIconFactory.get().setIcon(copyright, FontAwesomeIcon.COPYRIGHT);
        left.getChildren().addAll(new Label(String.format("ver. %s", pdfsam.property(VERSION))), copyright);
        addHyperlink(null, pdfsam.property(LICENSE_URL), pdfsam.property(LICENSE_NAME), left);
        addHyperlink(FontAwesomeIcon.HOME, pdfsam.property(HOME_URL), pdfsam.property(HOME_LABEL), left);
        addHyperlink(FontAwesomeIcon.RSS_SQUARE, pdfsam.property(FEED_URL),
                DefaultI18nContext.getInstance().i18n("Subscribe to the official news feed"), left);

        addSectionTitle(DefaultI18nContext.getInstance().i18n("Environment"), left);
        Label runtime = new Label(String.format("%s %s", System.getProperty("java.runtime.name"),
                System.getProperty("java.runtime.version")));
        Label fx = new Label(String.format(DefaultI18nContext.getInstance().i18n("JavaFX runtime version %s"),
                System.getProperty("javafx.runtime.version")));
        Label memory = new Label(DefaultI18nContext.getInstance().i18n("Max memory {0}",
                FileUtils.byteCountToDisplaySize(Runtime.getRuntime().maxMemory())));
        Button copyButton = new Button(DefaultI18nContext.getInstance().i18n("Copy to clipboard"));
        FontAwesomeIconFactory.get().setIcon(copyButton, FontAwesomeIcon.COPY);
        copyButton.getStyleClass().addAll(Style.BUTTON.css());
        copyButton.setId("copyEnvDetails");
        copyButton.setOnAction(a -> {
            ClipboardContent content = new ClipboardContent();
            writeContent(Arrays.asList(pdfsam.name(), pdfsam.property(VERSION), runtime.getText(), memory.getText()))
                    .to(content);
            Clipboard.getSystemClipboard().setContent(content);
        });
        left.getChildren().addAll(runtime, fx, memory, copyButton);

        VBox right = new VBox(5);
        addSectionTitle(DefaultI18nContext.getInstance().i18n("Support"), right);
        addHyperlink(FontAwesomeIcon.BUG, pdfsam.property(TRACKER_URL),
                DefaultI18nContext.getInstance().i18n("Bug and feature requests"), right);
        addHyperlink(FontAwesomeIcon.QUESTION_CIRCLE, pdfsam.property(SUPPORT_URL),
                DefaultI18nContext.getInstance().i18n("Support"), right);
        addHyperlink(FontAwesomeIcon.BOOK, pdfsam.property(DOCUMENTATION_URL),
                DefaultI18nContext.getInstance().i18n("Documentation"), right);

        addSectionTitle(DefaultI18nContext.getInstance().i18n("Contribute"), right);
        addHyperlink(FontAwesomeIcon.GITHUB, pdfsam.property(SCM_URL),
                DefaultI18nContext.getInstance().i18n("Fork PDFsam on GitHub"), right);
        addHyperlink(FontAwesomeIcon.FLAG_ALT, pdfsam.property(TRANSLATE_URL),
                DefaultI18nContext.getInstance().i18n("Translate"), right);
        addHyperlink(FontAwesomeIcon.DOLLAR, pdfsam.property(DONATE_URL),
                DefaultI18nContext.getInstance().i18n("Donate"), right);

        addSectionTitle(DefaultI18nContext.getInstance().i18n("Social"), right);
        addHyperlink(FontAwesomeIcon.TWITTER_SQUARE, pdfsam.property(TWITTER_URL),
                DefaultI18nContext.getInstance().i18n("Follow us on Twitter"), right);
        addHyperlink(FontAwesomeIcon.FACEBOOK_SQUARE, pdfsam.property(FACEBOOK_URL),
                DefaultI18nContext.getInstance().i18n("Like us on Facebook"), right);
        getChildren().addAll(left, right);
    }

    private void addSectionTitle(String title, Pane pane) {
        Label label = new Label(title);
        label.getStyleClass().add("section-title");
        pane.getChildren().add(label);
    }

    private void addHyperlink(FontAwesomeIcon icon, String url, String text, Pane pane) {
        UrlButton button = UrlButton.styledUrlButton(text, url, icon);
        button.getStyleClass().setAll("pdfsam-hyperlink");
        pane.getChildren().add(button);
    }
}
