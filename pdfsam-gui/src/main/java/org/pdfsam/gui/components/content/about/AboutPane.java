/*
 * This file is part of the PDF Split And Merge source code
 * Created on 21/ott/2013
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.content.about;

import jakarta.inject.Inject;
import javafx.scene.AccessibleRole;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import org.apache.commons.io.FileUtils;
import org.kordamp.ikonli.Ikon;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.ui.components.commons.UrlButton;
import org.pdfsam.ui.components.support.Style;
import org.sejda.core.Sejda;

import java.util.Arrays;

import static org.pdfsam.core.BrandableProperty.BLUESKY_URL;
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
import static org.pdfsam.core.BrandableProperty.VENDOR_URL;
import static org.pdfsam.core.BrandableProperty.VERSION;
import static org.pdfsam.core.support.io.ObjectCollectionWriter.writeContent;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Panel displaying information about the application
 *
 * @author Andrea Vacondio
 */
public class AboutPane extends HBox {

    @Inject
    public AboutPane(AppBrand appBrand) {
        getStyleClass().add("spaced-container");
        getStyleClass().add("about-panel");
        var left = new VBox(6);
        addSectionTitle(appBrand.property(BrandableProperty.NAME, "PDFsam Basic"), left);
        left.getChildren().addAll(new Label(String.format("ver. %s", appBrand.property(VERSION))));
        addHyperlink(UniconsLine.COPYRIGHT, appBrand.property(VENDOR_URL), appBrand.property(COPYRIGHT), left);
        addHyperlink(UniconsLine.BALANCE_SCALE, appBrand.property(LICENSE_URL), appBrand.property(LICENSE_NAME), left);
        addHyperlink(UniconsLine.HOME, appBrand.property(HOME_URL), appBrand.property(HOME_LABEL), left);
        addHyperlink(UniconsLine.RSS, appBrand.property(FEED_URL), i18n().tr("Subscribe to the official news feed"),
                left);

        addSectionTitle(i18n().tr("Environment"), left);
        var runtime = new Label(String.format("%s %s", System.getProperty("java.runtime.name"),
                System.getProperty("java.runtime.version")));
        var vendor = new Label(String.format(i18n().tr("Vendor: %s"), System.getProperty("java.vendor")));
        var runtimePath = new Label(String.format(i18n().tr("Java runtime path: %s"), System.getProperty("java.home")));
        var fx = new Label(
                String.format(i18n().tr("JavaFX runtime version: %s"), System.getProperty("javafx.runtime.version")));
        var sejda = new Label(String.format(i18n().tr("PDF engine (Sejda SDK) version: %s"), Sejda.VERSION));
        var memory = new Label(
                i18n().tr("Max memory {0}", FileUtils.byteCountToDisplaySize(Runtime.getRuntime().maxMemory())));
        Button copyButton = new Button(i18n().tr("Copy to clipboard"));
        var copyIcon = FontIcon.of(UniconsLine.COPY);
        copyIcon.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
        copyButton.setGraphic(copyIcon);
        copyButton.getStyleClass().addAll(Style.BUTTON.css());
        copyButton.setId("copyEnvDetails");
        copyButton.setOnAction(a -> {
            ClipboardContent content = new ClipboardContent();
            writeContent(
                    Arrays.asList(appBrand.property(BrandableProperty.NAME, "PDFsam Basic"), appBrand.property(VERSION),
                            runtime.getText(), vendor.getText(), runtimePath.getText(), fx.getText(),
                            memory.getText())).to(content);
            Clipboard.getSystemClipboard().setContent(content);
        });
        left.getChildren().addAll(runtime, vendor, runtimePath, fx, sejda, memory, copyButton);

        VBox right = new VBox(6);
        addSectionTitle(i18n().tr("Support"), right);
        addHyperlink(UniconsLine.BUG, appBrand.property(TRACKER_URL), i18n().tr("Bug and feature requests"), right);
        addHyperlink(UniconsLine.QUESTION_CIRCLE, appBrand.property(SUPPORT_URL), i18n().tr("Support"), right);
        addHyperlink(UniconsLine.BOOK, appBrand.property(DOCUMENTATION_URL), i18n().tr("Documentation"), right);

        addSectionTitle(i18n().tr("Contribute"), right);
        addHyperlink(UniconsLine.GITHUB, appBrand.property(SCM_URL), i18n().tr("Fork PDFsam on GitHub"), right);
        addHyperlink(UniconsLine.LANGUAGE, appBrand.property(TRANSLATE_URL), i18n().tr("Translate"), right);
        addHyperlink(UniconsLine.DOLLAR_ALT, appBrand.property(DONATE_URL), i18n().tr("Donate"), right);

        addSectionTitle(i18n().tr("Social"), right);
        addHyperlink(null, appBrand.property(BLUESKY_URL), i18n().tr("Follow us on Bluesky"), right);
        addHyperlink(UniconsLine.FACEBOOK, appBrand.property(FACEBOOK_URL), i18n().tr("Like us on Facebook"), right);
        getChildren().addAll(left, right);
    }

    private void addSectionTitle(String title, Pane pane) {
        Label label = new Label(title);
        label.getStyleClass().add("section-title");
        pane.getChildren().add(label);
    }

    private UrlButton addHyperlink(Ikon icon, String url, String text, Pane pane) {
        UrlButton button = UrlButton.styledUrlButton(text, url, icon);
        button.getStyleClass().setAll("pdfsam-hyperlink");
        pane.getChildren().add(button);
        return button;
    }
}
