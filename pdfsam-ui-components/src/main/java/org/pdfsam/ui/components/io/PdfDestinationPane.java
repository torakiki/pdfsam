/*
 * This file is part of the PDF Split And Merge source code
 * Created on 25/nov/2013
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
package org.pdfsam.ui.components.io;

import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.TitledPane;
import javafx.scene.layout.GridPane;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.pdfsam.core.support.params.AbstractPdfOutputParametersBuilder;
import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.model.tool.ToolBound;
import org.pdfsam.model.ui.DefaultPdfVersionComboItem;
import org.pdfsam.model.ui.ResettableView;
import org.pdfsam.model.ui.SetDestinationRequest;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.ui.components.support.Style;
import org.pdfsam.ui.components.support.Views;
import org.sejda.model.output.CompressionPolicy;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.parameter.base.AbstractPdfOutputParameters;
import org.sejda.model.pdf.PdfVersion;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static java.util.Arrays.asList;
import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.model.ui.ComboItem.keyWithEmptyValue;
import static org.pdfsam.ui.components.support.Views.helpIcon;

/**
 * Panel letting the user select an output destination for generated Pdf document/s.
 *
 * @author Andrea Vacondio
 */
public class PdfDestinationPane extends DestinationPane implements ToolBound, RestorableView, ResettableView,
        TaskParametersBuildStep<AbstractPdfOutputParametersBuilder<? extends AbstractPdfOutputParameters>> {

    private final PdfVersionCombo version;
    private final PdfCompressionPolicyCombo compress;
    private Optional<CheckBox> discardBookmarks = empty();
    private final String toolBinding;

    public PdfDestinationPane(BrowsableField destination, String toolBinding,
            DestinationPanelFields... optionalFields) {
        this(destination, toolBinding, false, optionalFields);
    }

    public PdfDestinationPane(BrowsableField destination, String toolBinding, boolean expandAdvanced,
            DestinationPanelFields... optionalFields) {
        super(destination);
        destination.setId(toolBinding + ".destination");
        overwrite().setSelected(app().persistentSettings().get(BooleanPersistentProperty.OVERWRITE_OUTPUT));
        this.toolBinding = defaultString(toolBinding);
        var advancedPanel = new GridPane();

        compress = new PdfCompressionPolicyCombo(toolBinding);
        var compressionPolicyLabel = new Label(i18n().tr("Output files compression") + ":");
        compressionPolicyLabel.setLabelFor(compress);
        advancedPanel.add(compressionPolicyLabel, 0, 0);
        GridPane.setFillWidth(compress, true);
        compress.setMaxWidth(Double.POSITIVE_INFINITY);
        advancedPanel.add(compress, 1, 0);
        var helpIcon = helpIcon("""
                - %s: %s
                - %s: %s
                - %s: %s
                """.formatted(i18n().tr("Compress"),
                i18n().tr("all streams in the output PDF will be compressed by applying FlateDecode encoding"),
                i18n().tr("Neutral"), i18n().tr("streams are left unchanged from the original file"),
                i18n().tr("Uncompress"), i18n().tr(
                        "FlateDecode and LZWDecode filters will be removed from all streams. Useful for debugging or manual inspection")));
        advancedPanel.add(helpIcon, 2, 0);

        version = new PdfVersionCombo(toolBinding);
        var versionLabel = new Label(i18n().tr("Output PDF version:"));
        versionLabel.setLabelFor(version);
        advancedPanel.add(versionLabel, 0, 1);
        GridPane.setFillWidth(version, true);
        version.setMaxWidth(Double.POSITIVE_INFINITY);
        advancedPanel.add(version, 1, 1);
        advancedPanel.add(helpIcon(i18n().tr("PDF version for generated PDF files")), 2, 1);

        if (asList(optionalFields).contains(DestinationPanelFields.DISCARD_BOOKMARKS)) {
            var discardBookmarksField = new CheckBox(i18n().tr("Discard bookmarks"));
            advancedPanel.add(discardBookmarksField, 0, 2, 2, 1);
            advancedPanel.add(helpIcon(
                            i18n().tr("Tick the box if you don't want to retain any bookmark from the original PDF document")),
                    2, 2);
            discardBookmarksField.setAccessibleHelp(
                    i18n().tr("Tick the box if you don't want to retain any bookmark from the original PDF document"));
            discardBookmarksField.setId("discardBookmarksField");
            discardBookmarksField.setSelected(
                    app().persistentSettings().get(BooleanPersistentProperty.DISCARD_BOOKMARKS));
            discardBookmarks = Optional.of(discardBookmarksField);
        }

        TitledPane titledAdvanced = Views.titledPane(i18n().tr("Show advanced settings"), advancedPanel,
                "advanced-destination-pane");
        titledAdvanced.setExpanded(expandAdvanced);
        titledAdvanced.expandedProperty().addListener((o, oldval, newVal) -> {
            if (newVal) {
                titledAdvanced.setText(i18n().tr("Hide advanced settings"));
            } else {
                titledAdvanced.setText(i18n().tr("Show advanced settings"));
            }
        });
        getChildren().add(titledAdvanced);
        advancedPanel.getStyleClass().addAll(Style.CONTAINER.css());
        advancedPanel.getStyleClass().addAll(Style.GRID.css());
        eventStudio().addAnnotatedListeners(this);
    }

    public void enableSameAsSourceItem() {
        version.enableSameAsSourceItem();
    }

    @Override
    @EventStation
    public String toolBinding() {
        return toolBinding;
    }

    @EventListener
    public void setDestination(SetDestinationRequest event) {
        if (!event.fallback() || (isBlank(destination().getTextField().getText()) && app().persistentSettings()
                .get(BooleanPersistentProperty.SMART_OUTPUT))) {
            destination().setTextFromFile(event.footprint());
        }
    }

    @Override
    public void resetView() {
        super.resetView();
        version.resetView();
        compress.resetView();
        overwrite().setSelected(app().persistentSettings().get(BooleanPersistentProperty.OVERWRITE_OUTPUT));
        discardBookmarks.ifPresent(
                c -> c.setSelected(app().persistentSettings().get(BooleanPersistentProperty.DISCARD_BOOKMARKS)));
    }

    @Override
    public void apply(AbstractPdfOutputParametersBuilder<? extends AbstractPdfOutputParameters> builder,
            Consumer<String> onError) {
        builder.compressionPolicy(compress.getSelectionModel().getSelectedItem().key());
        if (overwrite().isSelected()) {
            builder.existingOutput(ExistingOutputPolicy.OVERWRITE);
        }
        builder.version(version.getSelectionModel().getSelectedItem().getVersion());
        discardBookmarks.ifPresent(d -> builder.discardBookmarks(d.isSelected()));
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put("compressionPolicy", compress.getSelectionModel().getSelectedItem().key().name());
        data.put("overwrite", Boolean.toString(overwrite().isSelected()));
        discardBookmarks.ifPresent(d -> data.put("discardBookmarks", Boolean.toString(d.isSelected())));
        data.put("version", version.getSelectionModel().getSelectedItem().getVersion().toString());
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        version.resetView();
        var compressionPolicyString = data.get("compressionPolicy");
        if (isNotBlank(compressionPolicyString)) {
            this.compress.getSelectionModel()
                    .select(keyWithEmptyValue(CompressionPolicy.valueOf(compressionPolicyString)));
            //backward compatible when compress was a checkbox
        } else if (Boolean.parseBoolean(data.get("compress"))) {
            this.compress.getSelectionModel().select(keyWithEmptyValue(CompressionPolicy.COMPRESS));
        } else {
            this.compress.getSelectionModel().select(keyWithEmptyValue(CompressionPolicy.NEUTRAL));
        }
        overwrite().setSelected(Boolean.parseBoolean(data.get("overwrite")));
        discardBookmarks.ifPresent(d -> d.setSelected(Boolean.parseBoolean(data.get("discardBookmarks"))));
        ofNullable(data.get("version")).map(PdfVersion::valueOf).map(DefaultPdfVersionComboItem::new)
                .ifPresent(v -> this.version.getSelectionModel().select(v));
    }

    PdfVersionCombo getVersion() {
        return version;
    }

    PdfCompressionPolicyCombo getCompress() {
        return compress;
    }

    public enum DestinationPanelFields {
        DISCARD_BOOKMARKS
    }
}
