/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/nov/2013
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
package org.pdfsam.ui.io;

import static java.util.Arrays.asList;
import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.pdfsam.support.RequireUtils.requireNotNull;
import static org.pdfsam.ui.help.HelpUtils.helpIcon;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.UserContext;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.support.params.AbstractPdfOutputParametersBuilder;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.ResettableView;
import org.pdfsam.ui.commons.SetDestinationRequest;
import org.pdfsam.ui.io.PdfVersionCombo.DefaultPdfVersionComboItem;
import org.pdfsam.ui.support.Style;
import org.pdfsam.ui.support.Views;
import org.pdfsam.ui.workspace.RestorableView;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.model.output.ExistingOutputPolicy;
import org.sejda.model.parameter.base.AbstractPdfOutputParameters;
import org.sejda.model.pdf.PdfVersion;

import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.TitledPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

/**
 * Panel letting the user select an output destination for generated Pdf document/s.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PdfDestinationPane extends DestinationPane implements ModuleOwned, RestorableView, ResettableView,
        TaskParametersBuildStep<AbstractPdfOutputParametersBuilder<? extends AbstractPdfOutputParameters>> {

    private PdfVersionCombo version;
    private PdfVersionConstrainedCheckBox compress;
    private Optional<CheckBox> discardBookmarks = empty();
    private String ownerModule = StringUtils.EMPTY;
    private UserContext userContext;

    public PdfDestinationPane(BrowsableField destination, String ownerModule, UserContext userContext,
            DestinationPanelFields... optionalFields) {
        this(destination, ownerModule, userContext, false, optionalFields);
    }

    public PdfDestinationPane(BrowsableField destination, String ownerModule, UserContext userContext,
            boolean expandAdvanced, DestinationPanelFields... optionalFields) {
        super(destination);
        destination.setId(ownerModule + ".destination");
        requireNotNull(userContext, "UserContext cannot be null");
        this.userContext = userContext;
        this.ownerModule = defaultString(ownerModule);
        VBox advancedPanel = new VBox();
        advancedPanel.getStyleClass().addAll(Style.CONTAINER.css());
        version = new PdfVersionCombo(ownerModule);
        compress = new PdfVersionConstrainedCheckBox(PdfVersion.VERSION_1_5, ownerModule);
        compress.setText(DefaultI18nContext.getInstance().i18n("Compress output file/files"));
        compress.setSelected(userContext.isCompressionEnabled());
        compress.setId("compressField");
        compress.getStyleClass().addAll(Style.VITEM.css());

        if (asList(optionalFields).contains(DestinationPanelFields.DISCARD_BOOKMARKS)) {
            CheckBox discardBookmarksField = new CheckBox(DefaultI18nContext.getInstance().i18n("Discard bookmarks"));
            discardBookmarksField.setGraphic(helpIcon(DefaultI18nContext.getInstance()
                    .i18n("Tick the box if you don't want to retain any bookmark from the original PDF document")));
            discardBookmarksField.getStyleClass().addAll(Style.WITH_HELP.css());
            discardBookmarksField.getStyleClass().addAll(Style.VITEM.css());
            discardBookmarksField.setId("discardBookmarksField");
            discardBookmarks = Optional.of(discardBookmarksField);
        }
        HBox versionPane = new HBox(new Label(DefaultI18nContext.getInstance().i18n("Output PDF version:")), version);
        versionPane.getStyleClass().addAll(Style.VITEM.css());
        versionPane.getStyleClass().addAll(Style.HCONTAINER.css());
        advancedPanel.getChildren().add(compress);
        discardBookmarks.ifPresent(advancedPanel.getChildren()::add);
        advancedPanel.getChildren().add(versionPane);
        TitledPane titledAdvanced = Views.titledPane(DefaultI18nContext.getInstance().i18n("Show advanced settings"),
                advancedPanel);
        titledAdvanced.getStyleClass().add("advanced-destination-pane");
        titledAdvanced.setExpanded(expandAdvanced);
        titledAdvanced.expandedProperty().addListener((o, oldval, newVal) -> {
            if (newVal) {
                titledAdvanced.setText(DefaultI18nContext.getInstance().i18n("Hide advanced settings"));
            } else {
                titledAdvanced.setText(DefaultI18nContext.getInstance().i18n("Show advanced settings"));
            }
        });
        getChildren().add(titledAdvanced);
        eventStudio().addAnnotatedListeners(this);
    }

    public void enableSameAsSourceItem() {
        version.enableSameAsSourceItem();
    }

    @Override
    @EventStation
    public String getOwnerModule() {
        return ownerModule;
    }

    @EventListener
    public void setDestination(SetDestinationRequest event) {
        if (!event.isFallback()
                || (isBlank(destination().getTextField().getText()) && userContext.isUseSmartOutput())) {
            destination().setTextFromFile(event.getFootprint());
        }
    }

    @Override
    public void resetView() {
        super.resetView();
        version.resetView();
        compress.setSelected(false);
        compress.setSelected(true);
        discardBookmarks.ifPresent(c -> c.setSelected(false));
    }

    @Override
    public void apply(AbstractPdfOutputParametersBuilder<? extends AbstractPdfOutputParameters> builder,
            Consumer<String> onError) {
        builder.compress(compress.isSelected());
        if (overwrite().isSelected()) {
            builder.existingOutput(ExistingOutputPolicy.OVERWRITE);
        }
        builder.version(version.getSelectionModel().getSelectedItem().getVersion());
        discardBookmarks.ifPresent(d -> {
            builder.discardBookmarks(d.isSelected());
        });
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put("compress", Boolean.toString(compress.isSelected()));
        data.put("overwrite", Boolean.toString(overwrite().isSelected()));
        discardBookmarks.ifPresent(d -> {
            data.put("discardBookmarks", Boolean.toString(d.isSelected()));
        });
        data.put("version", version.getSelectionModel().getSelectedItem().getVersion().toString());
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        version.resetView();
        compress.setSelected(Boolean.valueOf(data.get("compress")));
        overwrite().setSelected(Boolean.valueOf(data.get("overwrite")));
        discardBookmarks.ifPresent(d -> {
            d.setSelected(Boolean.valueOf(data.get("discardBookmarks")));
        });
        ofNullable(data.get("version")).map(PdfVersion::valueOf).map(DefaultPdfVersionComboItem::new)
                .ifPresent(v -> this.version.getSelectionModel().select(v));
    }

    PdfVersionCombo getVersion() {
        return version;
    }

    PdfVersionConstrainedCheckBox getCompress() {
        return compress;
    }

    public static enum DestinationPanelFields {
        DISCARD_BOOKMARKS;
    }
}
