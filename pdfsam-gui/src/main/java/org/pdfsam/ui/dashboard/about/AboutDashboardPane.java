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

import static org.pdfsam.core.BrandableProperty.COPYRIGHT;
import static org.pdfsam.core.BrandableProperty.DOCUMENTATION_URL;
import static org.pdfsam.core.BrandableProperty.DONATE_URL;
import static org.pdfsam.core.BrandableProperty.FACEBOOK_URL;
import static org.pdfsam.core.BrandableProperty.FEED_URL;
import static org.pdfsam.core.BrandableProperty.HOME_LABEL;
import static org.pdfsam.core.BrandableProperty.HOME_URL;
import static org.pdfsam.core.BrandableProperty.LICENSE_NAME;
import static org.pdfsam.core.BrandableProperty.LICENSE_URL;
import static org.pdfsam.core.BrandableProperty.SCM_URL;
import static org.pdfsam.core.BrandableProperty.SUPPORT_URL;
import static org.pdfsam.core.BrandableProperty.TRACKER_URL;
import static org.pdfsam.core.BrandableProperty.TRANSLATE_URL;
import static org.pdfsam.core.BrandableProperty.TWITTER_URL;
import static org.pdfsam.core.BrandableProperty.VERSION;
import static org.pdfsam.core.support.io.ObjectCollectionWriter.writeContent;

import java.util.Arrays;

import javax.inject.Inject;

import org.apache.commons.io.FileUtils;
import org.pdfsam.core.AppBrand;
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
    public AboutDashboardPane(AppBrand appBrand) {
        getStyleClass().add("dashboard-container");
        VBox left = new VBox(5);
        addSectionTitle(appBrand.name(), left);
        Label copyright = new Label(appBrand.property(COPYRIGHT));
        FontAwesomeIconFactory.get().setIcon(copyright, FontAwesomeIcon.COPYRIGHT);
        left.getChildren().addAll(new Label(String.format("ver. %s", appBrand.property(VERSION))), copyright);
        addHyperlink(null, appBrand.property(LICENSE_URL), appBrand.property(LICENSE_NAME), left);
        addHyperlink(FontAwesomeIcon.HOME, appBrand.property(HOME_URL), appBrand.property(HOME_LABEL), left);
        addHyperlink(FontAwesomeIcon.RSS_SQUARE, appBrand.property(FEED_URL),
                i18n().tr("Subscribe to the official news feed"), left);

        addSectionTitle(i18n().tr("Environment"), left);
        Label runtime = new Label(String.format("%s %s", System.getProperty("java.runtime.name"),
                System.getProperty("java.runtime.version")));
        Label vendor = new Label(String.format(i18n().tr("Vendor: %s"), System.getProperty("java.vendor")));
        Label runtimePath = new Label(
                String.format(i18n().tr("Java runtime path: %s"), System.getProperty("java.home")));
        Label fx = new Label(
                String.format(i18n().tr("JavaFX runtime version %s"), System.getProperty("javafx.runtime.version")));
        Label memory = new Label(
                i18n().tr("Max memory {0}", FileUtils.byteCountToDisplaySize(Runtime.getRuntime().maxMemory())));
        Button copyButton = new Button(i18n().tr("Copy to clipboard"));
        FontAwesomeIconFactory.get().setIcon(copyButton, FontAwesomeIcon.COPY);
        copyButton.getStyleClass().addAll(Style.BUTTON.css());
        copyButton.setId("copyEnvDetails");
        copyButton.setOnAction(a -> {
            ClipboardContent content = new ClipboardContent();
            writeContent(Arrays.asList(appBrand.name(), appBrand.property(VERSION), runtime.getText(), vendor.getText(),
                    runtimePath.getText(), fx.getText(), memory.getText())).to(content);
            Clipboard.getSystemClipboard().setContent(content);
        });
        left.getChildren().addAll(runtime, vendor, runtimePath, fx, memory, copyButton);

        VBox right = new VBox(5);
        addSectionTitle(i18n().tr("Support"), right);
        addHyperlink(FontAwesomeIcon.BUG, appBrand.property(TRACKER_URL), i18n().tr("Bug and feature requests"), right);
        addHyperlink(FontAwesomeIcon.QUESTION_CIRCLE, appBrand.property(SUPPORT_URL), i18n().tr("Support"), right);
        addHyperlink(FontAwesomeIcon.BOOK, appBrand.property(DOCUMENTATION_URL), i18n().tr("Documentation"), right);

        addSectionTitle(i18n().tr("Contribute"), right);
        addHyperlink(FontAwesomeIcon.GITHUB, appBrand.property(SCM_URL), i18n().tr("Fork PDFsam on GitHub"), right);
        addHyperlink(FontAwesomeIcon.FLAG_ALT, appBrand.property(TRANSLATE_URL), i18n().tr("Translate"), right);
        addHyperlink(FontAwesomeIcon.DOLLAR, appBrand.property(DONATE_URL), i18n().tr("Donate"), right);

        addSectionTitle(i18n().tr("Social"), right);
        addHyperlink(FontAwesomeIcon.TWITTER_SQUARE, appBrand.property(TWITTER_URL), i18n().tr("Follow us on Twitter"),
                right);
        addHyperlink(FontAwesomeIcon.FACEBOOK_SQUARE, appBrand.property(FACEBOOK_URL), i18n().tr("Like us on Facebook"),
                right);
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
